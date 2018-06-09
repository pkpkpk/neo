(ns neo.vec.euler
  (:require [neo.math :as m :include-macros true]
            [neo.vec.mat4 :as mat4]))

;; orders => [ 'XYZ', 'YZX', 'ZXY', 'XZY', 'YXZ', 'ZYX' ];
; reorder

(defn setFromRotationMatrix
  ([this m](setFromRotationMatrix this m (.-order this) false))
  ([this m order](setFromRotationMatrix this m order false))
  ([this m order update]
   (let [m00 (aget m 0)
         m10 (aget m 1)
         m20 (aget m 2)
         m01 (aget m 4)
         m11 (aget m 5)
         m21 (aget m 6)
         m02 (aget m 8)
         m12 (aget m 9)
         m22 (aget m 10)
         order (or order this.order)]
     (assert (= order "XYZ"))
     (when (= order "XYZ")
       (set! this.y (m/asin (goog.math.clamp m02 -1 1)))
       (if (< (m/abs m02) 0.99999)
         (do
           (set! this.x (m/atan2 (- m12) m22))
           (set! this.z (m/atan2 (- m01) m00)))
         (do
           (set! this.x (m/atan2 m21 m11))
           (set! this.z 0))))
     (set! this.order order)
     (when-not (false? update) (.onChangeCallback this))
     this)))

(def setFromQuaternion
  (let [matrix (mat4/mat4)]
    (fn
      ([this q]
       (mat4/makeRotationFromQuaternion matrix q)
       (setFromRotationMatrix this matrix "XYZ" false))
      ([this q order]
       (mat4/makeRotationFromQuaternion matrix q)
       (setFromRotationMatrix this matrix order false))
      ([this q order update]
       (mat4/makeRotationFromQuaternion matrix q)
       (setFromRotationMatrix this matrix order update)))))

(deftype Euler [x y z order]
  Object
  (onChange [this cb] (set! (.-onChangeCallback this) cb) this)
  (onChangeCallback [this]))

(defn euler
  "@return {!Euler}"
  ([] (Euler. 0 0 0 "XYZ"))
  ([x y z] (Euler. x y z "XYZ"))
  ([x y z order] (Euler. x y z order)))

(defn eclone
  "@param {!Euler}
   @return {!Euler}"
  [e]
  (Euler. (.-x e) (.-y e) (.-z e) (.-order e)))

(defn setFromValues
  ([e x y z]
   (set! (.-x e) x)
   (set! (.-y e) y)
   (set! (.-z e) z)
   e)
  ([e x y z order]
   (set! (.-x e) x)
   (set! (.-y e) y)
   (set! (.-z e) z)
   (set! (.-order e) order)
   e))

(defn copy
  "@param {!Euler} a
   @param {!Euler} b
   @return {!Euler}"
  [a b]
  (set! (.-x a) (.-x b))
  (set! (.-y a) (.-y b))
  (set! (.-z a) (.-z b))
  (set! (.-order a) (.-order b))
  a)

