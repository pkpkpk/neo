(ns neo.test-helpers
  (:require-macros [neo.macros :refer [vx vy vz vw vsetz vsetz+]])
  (:require [cljs.test :refer-macros [is]]
            [neo.math :as m :include-macros true]
            [goog.vec]))

(def ^:dynamic *eps* goog.vec.EPSILON)


;needed to destructure
(extend-type js/Float64Array
  IIndexed
  (-nth
   ([arr n]
    (if (and (<= 0 n) (< n (.-length arr)))
      (aget arr n)
      (throw  (js/Error. "Index out of bounds"))))
   ([arr n not-found]
    (if (and (<= 0 n) (< n (.-length arr)))
      (aget arr n)
      not-found))))

(extend-type js/Float32Array
  IIndexed
  (-nth
   ([arr n]
    (if (and (<= 0 n) (< n (.-length arr)))
      (aget arr n)
      (throw  (js/Error. "Index out of bounds"))))
   ([arr n not-found]
    (if (and (<= 0 n) (< n (.-length arr)))
      (aget arr n)
      not-found))))

(defn is=
  ([a b] (is (= a b)))
  ([a b c] (is (= a b c)))
  ([a b c d] (is (= a b c d))))

(defn roughly=
  ([a b](roughly= a b *eps*))
  ([a b tolerance]
   (assert (and (number? a) (number? b)) "roughly= takes numbers")
   (assert (number? tolerance) "roughly= tolerance is NaN")
   (< (m/abs (- a b)) tolerance)))

(defn ->seq [s]
  (if (goog.isArrayLike s)
    (array-seq s)
    (seq s)))

(defn ^boolean elements-roughly=
  "predicate"
  ([a b]
   (let [a (->seq a)
         b (->seq b)]
     (and (= (count a) (count b))
          (every? true? (map roughly= a b (repeat *eps*))))))
  ([a b c]
   (let [a (->seq a)
         b (->seq b)
         c (->seq c)]
     (and  (= (count a) (count b) (count c))
           (every? true? (map roughly= a b (repeat *eps*)))
           (every? true? (map roughly= a c (repeat *eps*)))
           (every? true? (map roughly= b c (repeat *eps*))))))
  ([a b c d]
   (let [a (->seq a)
         b (->seq b)
         c (->seq c)
         d (->seq d)]
     (and  (= (count a) (count b) (count c) (count d))
           (every? true? (map roughly= a b (repeat *eps*)))
           (every? true? (map roughly= a c (repeat *eps*)))
           (every? true? (map roughly= a d (repeat *eps*)))
           (every? true? (map roughly= b c (repeat *eps*)))
           (every? true? (map roughly= b d (repeat *eps*)))
           (every? true? (map roughly= c d (repeat *eps*)))))))

(defn are-elements-roughly=
  "built in test assertion"
  ([a b] (is (elements-roughly= a b)))
  ([a b c] (is (elements-roughly= a b c)))
  ([a b c d] (is (elements-roughly= a b c d))))

(defn is-euler-roughly=
  [a b]
  (and (is (= (.-order a) (.-order b)))
       (are-elements-roughly= [(.-x a) (.-y a) (.-z a)]
                              [(.-x b) (.-y b) (.-z b)])))
