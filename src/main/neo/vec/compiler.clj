(ns neo.vec.compiler
  (:require [clojure.set :as s]))

(def mat4-coords   '[m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33])
(def affine-coords '[m00 m10 m20     m01 m11 m21     m02 m12 m22     m03 m13 m23])

(defn alter-prefix
 ([prefix sym]
  (symbol (str prefix (subs (str sym) 1))))
 ([prefix sym suffix]
  (symbol (str prefix (subs (str sym) 1) suffix))))

(def asyms-16 (mapv #(alter-prefix "a" %) mat4-coords))
(def asyms-12 (mapv #(alter-prefix "a" %) affine-coords))

(def bsyms-16 (mapv #(alter-prefix "b" %) mat4-coords))
(def bsyms-12 (mapv #(alter-prefix "b" %) affine-coords))

(defn mult4-template
  [as bs]
  ;; each row is a sum
  ;; each vec in a row is a *
  (assert (= 16 (count as) (count bs)) "mult4 template requires 16 length vectors for 4x4")
  (let [[a00 a10 a20 a30  a01 a11 a21 a31  a02 a12 a22 a32  a03 a13 a23 a33] as
        [b00 b10 b20 b30  b01 b11 b21 b31  b02 b12 b22 b32  b03 b13 b23 b33] bs]
    [[[a00 b00]  [ a01 b10]  [ a02 b20]  [ a03 b30]]
     [[a10 b00]  [ a11 b10]  [ a12 b20]  [ a13 b30]]
     [[a20 b00]  [ a21 b10]  [ a22 b20]  [ a23 b30]]
     [[a30 b00]  [ a31 b10]  [ a32 b20]  [ a33 b30]]
     [[a00 b01]  [ a01 b11]  [ a02 b21]  [ a03 b31]]
     [[a10 b01]  [ a11 b11]  [ a12 b21]  [ a13 b31]]
     [[a20 b01]  [ a21 b11]  [ a22 b21]  [ a23 b31]]
     [[a30 b01]  [ a31 b11]  [ a32 b21]  [ a33 b31]]
     [[a00 b02]  [ a01 b12]  [ a02 b22]  [ a03 b32]]
     [[a10 b02]  [ a11 b12]  [ a12 b22]  [ a13 b32]]
     [[a20 b02]  [ a21 b12]  [ a22 b22]  [ a23 b32]]
     [[a30 b02]  [ a31 b12]  [ a32 b22]  [ a33 b32]]
     [[a00 b03]  [ a01 b13]  [ a02 b23]  [ a03 b33]]
     [[a10 b03]  [ a11 b13]  [ a12 b23]  [ a13 b33]]
     [[a20 b03]  [ a21 b13]  [ a22 b23]  [ a23 b33]]
     [[a30 b03]  [ a31 b13]  [ a32 b23]  [ a33 b33]]]))

(defn ensure-16 [v]
  (assert (vector? v))
  (let [n (count v)]
    (if (= 16 n)
      v
      (do
        (assert (= n 12))
        [(v 0)  (v 1)  (v 2) 0
         (v 3)  (v 4)  (v 5) 0
         (v 6)  (v 7)  (v 8) 0
         (v 9) (v 10) (v 11) 1]))))

(defn ensure-12 [v]
  (assert (vector? v))
  (let [n (count v)]
    (if (= 12 n)
      v
      [(v 0)    (v 1)   (v 2)
       (v 4)    (v 5)   (v 6)
       (v 8)    (v 9)  (v 10)
       (v 12)  (v 13)  (v 14)])))

(defn z? [x] (and (number? x) (zero? x)))

(defn one? [x] (and (number? x) (= 1 x)))

(defn simplify-numerics [v]
  (into []
        (comp
         (map
          (fn [[a b]]
            (when-not (or (z? a) (z? b))
              (if (one? a)
                (if (one? b)
                  1
                  b)
                (if (one? b)
                  a
                  (if (and (number? a) (number? b))
                    (* a b)
                    [a b]))))))
         (remove nil?))
        v))

(defn replace-sexps [v]
  (let [form->sym (atom {})
        mult-term (fn mt [item]
                    (cond
                      (vector? item)
                      [(mt (first item)) (mt (second item))]

                      (list? item)
                      (or (get @form->sym item)
                          (if (and (= '- (first item)) (list? (second item)))
                            (if-let [sym (get @form->sym (second item))]
                              (or (get @form->sym (list '- sym))
                                  (let [sym' (gensym)]
                                    (swap! form->sym assoc (list '- sym) sym')
                                    sym'))
                              (let [sym (gensym)]
                                (swap! form->sym assoc (second item) sym)
                                (list '- sym)))
                            (let [sym (gensym)]
                              (swap! form->sym assoc item sym)
                              sym)))
                      :else
                      item))
        v' (mapv #(mapv mult-term %) v)]
    [v' @form->sym]))

(defn build-array-expr
  [write-sym {:keys [template bindings]} syms table]
  (let [bindings (or bindings [])
        observed-reads (atom #{})
        track (fn [x] ;; track minimal set of reads
                (when-let [{:keys [sym i]} (get table x)]
                  (let [expr (list 'aget sym i)]
                    (swap! observed-reads conj [x expr])))
                x)
        mult-expr
        (fn [x] ; x is entry in sum-row
          (if (vector? x)
            (let [[a b] x]
              (list '* (track a) (track b)))
            (track x)))
        sum-expr
        (fn [sumv] ;=> (+ ... )  |  symbol
          (let [mult-list (doall (map mult-expr sumv))]
            (if (= (count mult-list) 1)
              (first mult-list) ;;if single mult op, no sum op necessary
              (cons '+  mult-list))))
        set-expr
        (fn [[target sum]] ;=> ((aset a 0 (+ ...)), (aset a 1 (+ ...)),...)
          (let [index (get-in table [target :i])]
            (list 'aset write-sym index (sum-expr sum))))
        ;; filter out no change (..assignment to itself)
        ops (remove (fn [[k v]] (= [k] v)) (zipmap syms template)) ;; =>  {m00  [1 [m21 b] [c b]]}
        expr (doall (sort-by #(nth % 2) (map set-expr ops)))
        bindings (into bindings (apply concat (sort @observed-reads)))]
    `(let ~bindings
       ~@expr
       ~write-sym)))

(defn affine-template? [v]
  (and
   (= [1] (v 15))
   (= [] (v 3) (v 7) (v 11))))

(defn affine? [v] (and (= 1 (v 15)) (= 0 (v 3) (v 7) (v 11))))

(defn build-affine-template [as bs]
  {:pre [(affine? as) (affine? bs)]}
  (let [t (mapv simplify-numerics (mult4-template as bs))
        [t m] (replace-sexps t)]
    (assert (affine-template? t))
    {:template (ensure-12 t)
     :bindings (when m (vec (apply concat (s/map-invert m))))}))

(defn index-table
  [binding-symbol aliases]
  (into {}
        (map-indexed
         (fn [i alias]
           [alias {:i i :sym binding-symbol}])
         aliases)))

(defmacro affine-mult
  "accepts a 16-length vector (or symbols) representing a 4x4 matrix and uses
   compile-time numbers to reduce a full multiplication down to the minimum
   amount of computations."
  [a b]
  (assert (or (symbol? a) (vector? a)))
  (assert (or (symbol? b) (vector? b)))
  (let [asyms asyms-12
        bsyms bsyms-12
        ; we want 16 so we can use full 4x4 template, verify inputs are affine
        as (ensure-16 (if (vector? a) a asyms))
        bs (ensure-16 (if (vector? b) b bsyms))
        template (build-affine-template as bs)]
    (cond
      (and (not (vector? a)) (vector? b))
      (let [table (index-table a asyms)]
        (build-array-expr a template asyms table))

      (and (vector? a) (not (vector? b)))
      (let [table (index-table b bsyms)]
        (build-array-expr b template bsyms table))

      :else
      (let [table (merge (index-table a asyms)
                         (index-table b bsyms))]
        (build-array-expr b template bsyms table)))))

(defmacro ->affine-array
  "compile time check vec"
  [v]
  (assert (affine? v))
  (let [[m00 m10 m20 _  m01 m11 m21 _ m02 m12 m22 _ m03 m13 m23] v]
    `(~'createFromValues  ~m00 ~m10 ~m20  ~m01 ~m11 ~m21  ~m02 ~m12 ~m22  ~m03 ~m13 ~m23)))

(defmacro affine-template [a b]
  (let [as (ensure-16 (if (vector? a) a asyms-12))
        bs (ensure-16 (if (vector? b) b bsyms-12))
        template (build-affine-template as bs)
        ; ops (remove (fn [[k v]] (= k v)) (zipmap asyms-12 template))
        ]
    `(quote ~template)))

(defn build-mat4-template [as bs]
  (let [t (mapv simplify-numerics (mult4-template as bs))
        [t m] (replace-sexps t)]
    {:template t
     :bindings (when m (vec (apply concat (s/map-invert m))))}))

(defmacro mat4-mult
  "accepts a 16-length vector (or symbols) representing a 4x4 matrix and uses
   compile-time numbers to reduce a full multiplication down to the minimum
   amount of computations."
  [a b]
  (assert (or (symbol? a) (vector? a)))
  (assert (or (symbol? b) (vector? b)))
  (let [asyms asyms-16
        bsyms bsyms-16
        as (ensure-16 (if (vector? a) a asyms))
        bs (ensure-16 (if (vector? b) b bsyms))
        template (build-mat4-template as bs)]
    (cond
      (and (not (vector? a)) (vector? b))
      (let [table (index-table a asyms)]
        (build-array-expr a template asyms table))

      (and (vector? a) (not (vector? b)))
      (let [table (index-table b bsyms)]
        (build-array-expr b template bsyms table))

      :else
      (let [table (merge (index-table a asyms)
                         (index-table b bsyms))]
        (build-array-expr b template bsyms table)))))

