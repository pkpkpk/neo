(ns neo.util)

(defn map!
  "maps f or second arg array, mutating in place"
  [f arr]
  (assert (goog.isArrayLike arr))
  (let [stop (alength arr)]
    (loop [i 0]
      (when (< i stop)
        (aset arr i (f (aget arr i)))
        (recur (inc i))))))

(defn map-indexed!
  [f arr]
  (assert (goog.isArrayLike arr))
  (let [stop (alength arr)]
    (loop [i 0]
      (when (< i stop)
        (aset arr i (f i (aget arr i)))
        (recur (inc i))))))

(def ^:dynamic *eps* 1e-10)

(defn ^number epsilon [n]
  (if (< (js/Math.abs n) *eps*)
    0
    n))

(defn applyEpsilon [arr]
  (map! epsilon arr))