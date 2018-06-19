(ns neo.vec.quaternion
  (:require-macros [neo.macros :refer [vx vy vz vw set* vsetx vsety vsetz vsetw
                                       vsetx* vsety* vsetz* vsetw* goog-typedef]])
  (:require [neo.math :as m :include-macros true]
            [goog.vec.Quaternion :as gq]))

(goog-define useFloat32 false)

(if ^boolean useFloat32
  (goog-typedef Quaternion "{goog.vec.Quaternion.Float32}")
  (goog-typedef Quaternion "{goog.vec.Quaternion.Float64}"))

(defn quaternion
  "@return {!Quaternion}"
  ([](quaternion 0 0 0 1))
  ([x y z w]
   (specify!
    (if ^boolean useFloat32
      (gq/createFloat32FromValues x y z w)
      (gq/createFloat64FromValues x y z w))
    IEquiv
    (-equiv [this q]
            (and (= (type this) (type q))
                 (= (vx this) (vx q))
                 (= (vy this) (vy q))
                 (= (vz this) (vz q))
                 (= (vw this) (vw q))))
    Object
    (onChange [this cb] (set! (.-onChangeCallback this) cb) this)
    (onChangeCallback [this]))))

(defn qclone [q] (quaternion (vx q) (vy q) (vz q) (vw q)))

(def setFromValues gq/setFromValues)

(defn copy
  "@param {!Quaternion} q
   @return {!Quaternion}"
  [this q]
  (vsetx this (vx q))
  (vsety this (vy q))
  (vsetz this (vz q))
  (vsetw this (vw q))
  (.onChangeCallback this)
  this)

(defn add
  ([a b]
   (gq/add a b a))
  ([a b result]
   (gq/add a b result)))

(def dot gq/dot)

(defn conjugate
  ([q]
   (gq/conjugate q q))
  ([q result]
   (gq/conjugate q result)))

(defn negate
  ([q]
   (gq/negate q q))
  ([q result]
   (gq/negate q result)))

(defn invert
  ([q]
   (gq/invert q q))
  ([q result]
   (gq/invert q result)))

(defn mult
  "@return {!Quaternion}"
  ([a b]
   (gq/concat a b a)
   (.onChangeCallback a)
   a)
  ([a b result]
    (gq/concat a b result)
    (.onChangeCallback result)
    result))

(defn premult
  "@return {!Quaternion}"
  ([a b]
   (let [q (gq/concat b a a)]
     (.onChangeCallback a)
     a))
  ([a b result]
   (let [q (gq/concat b a result)]
     (.onChangeCallback result)
     result)))

(defn scale
  ([q scalar]
   (gq/scale q scalar q))
  ([q scalar result]
   (gq/scale q scalar result)))

(defn rotateX
  ([q angle]
   (gq/rotateX q angle q))
  ([q angle result]
   (gq/rotateX q angle result)))

(defn rotateY
  ([q angle]
   (gq/rotateY q angle q))
  ([q angle result]
   (gq/rotateY q angle result)))

(defn rotateZ
  ([q angle]
   (gq/rotateZ q angle q))
  ([q angle result]
   (gq/rotateZ q angle result)))

(defn ^number angle-to-axis
  [q axis]
  (gq/toAngleAxis q axis))

(defn transform-vec ;; same as vec3 applyQuaternion => vec
  "Transforms a vec with a quaternion. Works on both vec3 and vec4
   @param {!goog.vec.AnyType} vec :: The vec to transform.
   @param {!Quaternion} quat
   @param {?goog.vec.AnyType} result :: The vec to receive the result.
   @return {!goog.vec.AnyType}"
  ([q vec]
   (gq/transformVec vec q vec))
  ([q vec result]
   (gq/transformVec vec q result)))

(defn length
  "@param {!Quaternion} q
   @return {!number}"
  [q]
  (gq/magnitude q))

(def lengthSq gq/magnitudeSquared)

(defn normalize
  "@param {!Quaternion} q
   @return {!Quaternion}"
  [q]
  (do
    (gq/normalize q q)
    (.onChangeCallback q)
    q))

(defn checked-normalize
  "checked normalize, safe for null vec
   @param {!Quaternion} q
   @return {!Quaternion}"
  [q]
  (if (== 0 (vx q) (vy q) (vz q) (vw q))
    q
    (normalize q)))

(defn setFromRotationMatrix
  "Generates the quaternion from the given 4x4 rotation matrix.
   @param {!Quaternion} quat :: The resulting quaternion..
   @param {!goog.vec.AnyType} matrix The source matrix.
   @return{!Quaternion}"
  [q m]
  (let [m00 (aget m 0)
        m10 (aget m 1)
        m20 (aget m 2)
        m01 (aget m 4)
        m11 (aget m 5)
        m21 (aget m 6)
        m02 (aget m 8)
        m12 (aget m 9)
        m22 (aget m 10)
        trace (+ m00 m11 m22)]
    (cond
      (< 0 trace)
      (let [s (/ 0.5 (m/sqrt (inc trace)))]
        (vsetw q (/ 0.25 s))
        (vsetx q (* s (- m21 m12)))
        (vsety q (* s (- m02 m20)))
        (vsetz q (* s (- m10 m01))))

      (and (< m11 m00) (< m22 m00))
      (let [s (* 2 (m/sqrt (- (inc m00) m11 m22)))]
        (vsetw q (/ (- m21 m12) s))
        (vsetx q (* 0.25 s))
        (vsety q (/ (+ m01 m10) s))
        (vsetz q (/ (+ m02 m20) s)))

      (< m22 m11)
      (let [s (* 2 (m/sqrt (- (inc m11) m00 m22)))]
        (vsetw q (/ (- m02 m20) s))
        (vsetx q (/ (+ m01 m10) s))
        (vsety q (* 0.25 s))
        (vsetz q (/ (+ m12 m21) s)))

      :else
      (let [s (* 2 (m/sqrt (- (inc m22) m00 m11)))]
        (vsetw q (/ (- m10 m01) s))
        (vsetx q (/ (+ m02 m20) s))
        (vsety q (/ (+ m12 m21) s))
        (vsetz q (* 0.25 s))))

    (.onChangeCallback q)
    q))


(defn mat4->quat [mat]
  (setFromRotationMatrix (quaternion) mat))

(defn setFromEuler
  "@return{!Quaternion}"
  ([this euler] (setFromEuler this euler false))
  ([this euler update]
   (let [x (.-x euler)
         y (.-y euler)
         z (.-z euler)
         order (or (.-order euler) (.-_order euler))
         _(assert (= "XYZ" order))
         c1 (m/cos (/ x 2))
         c2 (m/cos (/ y 2))
         c3 (m/cos (/ z 2))
         s1 (m/sin (/ x 2))
         s2 (m/sin (/ y 2))
         s3 (m/sin (/ z 2))]
     (vsetw this (- (* c1 c2 c3) (* s1 s2 s3)))
     (vsetx this (+ (* s1 c2 c3) (* c1 s2 s3)))
     (vsety this (- (* c1 s2 c3) (* s1 c2 s3)))
     (vsetz this (+ (* c1 c2 s3) (* s1 s2 c3)))
     (when-not (false? update) (.onChangeCallback this))
     this)))

(defn setFromAxisAngle
  "assumes axis is normalized
   @param{!Quaternion} q
   @param{!goog.vec.vec3d} axis
   @param{!number} angle
   @return{!Quaternion}"
  [q axis angle]
  (let [ha (* 0.5 angle)
        sin (m/sin ha)]
    (gq/setFromValues q
                      (* sin (vx axis))
                      (* sin (vy axis))
                      (* sin (vz axis))
                      (m/cos ha))
    (.onChangeCallback q)
    q))

