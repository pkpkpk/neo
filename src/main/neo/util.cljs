(ns neo.util)

(defn map!
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