(ns neo.vec.mat4
  (:require-macros [neo.macros :refer [vx vy vz vw vsetz vsetz+ vsetx+ aset* goog-typedef]]
                   [neo.vec.compiler :as c])
  (:require [neo.math :as m :include-macros true :refer [sin cos tan]]
            [neo.util :refer [epsilon]]
            [goog.vec.Mat4 :as gmat]
            [goog.vec.Quaternion :as gq]
            [neo.vec.vec3 :as vec3]
            [neo.vec.quaternion :as quat]))

(goog-define useFloat32 false)

(if ^boolean useFloat32
  (goog-typedef Mat4 "{goog.vec.Mat4.Float32}")
  (goog-typedef Mat4 "{goog.vec.Mat4.Float64}"))

(def create
  (if ^boolean useFloat32
    gmat/createFloat32
    gmat/createFloat64))

(defn ident
  "Creates a 4x4 identity matrix.
   @param {?Mat4} mat
   @return {!Mat4} The new 16 element array."
  ([]
   (if ^boolean useFloat32
     (gmat/createFloat32Identity)
     (gmat/createFloat64Identity)))
  ([mat]
   (gmat/makeIdentity mat)))

(defn mat4 [] (ident))

(def createFromValues
  (if ^boolean useFloat32
    gmat/createFloat32FromValues
    gmat/createFloat64FromValues))

(def setFromValues gmat/setFromValues)

(def copy gmat/setFromArray)

(def mclone
  (if ^boolean useFloat32
    gmat/createFloat32FromArray
    gmat/createFloat64FromArray))

(defn transpose
  "Transposes the given matrix mat storing the result into resultMat.
   @param {Mat4} mat :: The matrix to transpose
   @param {?Mat4} result :: defaults to first arg.
   @return {goog.vec.Mat4.AnyType} result"
  ([mat](gmat/transpose mat mat))
  ([mat result](gmat/transpose mat result)))

(defn getTranslation
  "Retrieves the translation component of the transformation matrix.
   @return {!goog.vec.Vec3.AnyType}"
  ([m] (vec3/createFromValues (aget m 12) (aget m 13) (aget m 14)))
  ([m v] (vec3/setFromValues v (aget m 12) (aget m 13) (aget m 14))))

(defn setPosition
  "@param {!Mat4} m
   @param {!Vec/Vec3}
   @return {!Mat4}"
  [m v]
  (do
    (aset m 12 (vx v))
    (aset m 13 (vy v))
    (aset m 14 (vz v))
    m))

(defn copyPosition
  "@param {!Mat4} a
   @param {!Mat4} b
   @return {!Mat4}"
  [a b]
  (do
    (aset a 12 (aget b 12))
    (aset a 13 (aget b 13))
    (aset a 14 (aget b 14))
    a))

(defn ^boolean invert
  "Computes the inverse of mat storing the result into a. If the inverse is
   defined, this function returns true, false otherwise.
   @param {Mat4} mat :: The matrix to invert.
   @param {?Mat4} result :: defaults to first arg.
   @return {boolean}"
  ([mat] (gmat/invert mat mat))
  ([mat result] (gmat/invert mat result)))

(defn determinant
  "Computes the determinant of the matrix.
   @param {Mat4} mat The matrix to compute the matrix for.
   @return {number} The determinant of the matrix."
  [mat]
  (gmat/determinant mat))

(defn mult
  "Multiply a and b using matrix multiplication
   @param {Mat4} a :: The first (left hand) matrix.
   @param {Mat4} b :: The second (right hand) matrix.
   @param {?Mat4} result :: defaults to first arg.
   @return {Mat4} return result"
  ([a b] (gmat/multMat a b a))
  ([a b result] (gmat/multMat a b result)))

(defn multScalar
  "Multiplies matrix mat with the given scalar
   @param {Mat4} mat :: The matrix to scale.
   @param {number} scalar ::  The scalar value to multiply to each element of mat.
   @param {?Mat4} result :: defaults to first arg
   @return {Mat4} return result"
  ([mat scalar] (gmat/multScalar mat scalar mat))
  ([mat scalar result] (gmat/multScalar mat scalar result)))


(defn translate
  ([x y z]
   (gmat/makeTranslate (create) x y z))
  ([mat x y z]
   (gmat/makeTranslate mat x y z)))

(defn translate!
  "shortcut for (mult m (translate x y z))
   @return {!Mat4} mutated affine transform."
  ([m x y](gmat/translate m x y 0))
  ([m x y z] (gmat/translate m x y z)))

(defn pre-translate!
  "Shortcut for (mult (translate x y z) m)
   @return {!(Float64Array|Float32Array)} mutated affine transform."
  ([m x y]
   (c/mat4-mult [1 0 0 0,  0 1 0 0,  0 0 1 0,  x y 0 1] m))
  ([m x y z]
   (c/mat4-mult [1 0 0 0,  0 1 0 0,  0 0 1 0,  x y z 1] m)))


(defn scale
  ([x y z] (gmat/makeScale (create) x y z))
  ([m x y z] (gmat/makeScale m x y z)))

(defn scale!
  "Shortcut for (mult m (scale x ?y ?z))
   - A single param will be used for both x + y
   @return {!Mat4} the mutated transform."
  ([m f] (gmat/scale m f f 1))
  ([m x y] (gmat/scale m x y 1))
  ([m x y z] (gmat/scale m x y z)))

(defn pre-scale!
  "Shortcut for (mult (scale x ?y ?z) m)
   - A single param will be used for both x + y
   @return {!Mat4} the mutated transform."
  ([m sx]
   (c/mat4-mult [sx 0 0 0, 0 sx 0 0, 0 0 1 0, 0 0 0 1] m))
  ([m sx sy]
   (c/mat4-mult [sx 0 0 0, 0 sy 0 0, 0 0 1 0, 0 0 0 1] m))
  ([m sx sy sz]
   (c/mat4-mult [sx 0 0 0, 0 sy 0 0, 0 0 sz 0, 0 0 0 1] m)))


(defn rotateX
  ([angle] (gmat/makeRotateX (create) angle))
  ([mat angle] (gmat/makeRotateX mat angle)))

(defn rotateX!
  "Rotate the given matrix by angle about the x axis.  Equivalent to:
     (mult m (rotateX theta))
   @param {!Mat4} m :: the matrix to mutate
   @param {!number} theta :: angle in radians
   @return {!Mat4} the mutated transform."
  [m theta]
  (gmat/rotateX m theta))

(defn rotateY
  ([angle] (gmat/makeRotateY (create) angle))
  ([mat angle] (gmat/makeRotateY mat angle)))

(defn rotateY!
  "Rotate the given matrix by angle about the y axis.  Equivalent to:
     (mult m (rotateY theta))
   @param {!Mat4} m :: the matrix to mutate
   @param {!number} theta :: angle in radians
   @return {!Mat4} the mutated transform."
  [m theta]
  (gmat/rotateY m theta))

(defn rotateZ
  ([angle] (gmat/makeRotateZ (create) angle))
  ([mat angle] (gmat/makeRotateZ mat angle)))

(defn rotateZ!
  "Rotate the given matrix by angle about the z axis. Equivalent to:
     (mult m (rotateZ theta))
   @param {!Mat4} m :: the matrix to mutate
   @param {!number} theta :: angle in radians
   @return {!Mat4} the mutated transform."
  [m theta]
  (gmat/rotateZ m theta))

(defn rotate-axis
  ([axis angle](gmat/makeRotate create angle (vx axis) (vy axis) (vz axis)))
  ([mat axis angle] (gmat/makeRotate mat angle (vx axis) (vy axis) (vz axis))))

; (defn rotate-axis! [m theta vec])


(defn shear
  "@return {!(Float64Array|Float32Array)} the shear matrix"
  ([psi theta phi] (shear (create) psi theta phi))
  ([m psi theta phi]
   (setFromValues m 1 (tan theta) 0 0, (tan psi) 1 0, 0 0 (tan phi) 1 0, 0 0 0 1)))

(defn shear!
  "shortcut for (mult m (shear phi theta psi))
   @return {!(Float64Array|Float32Array)}"
  ([m psi]
   (c/mat4-mult m [1 0 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1]))
  ([m psi theta]
   (c/mat4-mult m [1 (tan theta) 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1]))
  ([m psi theta phi]
   (c/mat4-mult m [1 (tan theta) 0 0, (tan psi) 1 0, 0 0 (tan phi) 1 0, 0 0 0 1])))

(defn pre-shear!
  "shortcut for (mult (shear phi theta psi) m)
   @return {!(Float64Array|Float32Array)}"
  ([m psi]
   (c/mat4-mult [1 0 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1] m))
  ([m psi theta]
   (c/mat4-mult [1 (tan theta) 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1] m))
  ([m psi theta phi]
   (c/mat4-mult [1 (tan theta) 0 0, (tan psi) 1 0, 0 0 (tan phi) 1 0, 0 0 0 1] m)))


(def extractRotation
  (let [v (vec3/vec3)]
    (fn [mat]
      (let [ret (ident)
            scaleX (/ 1 (vec3/length (vec3/setFromArray v mat 0)))
            scaleY (/ 1 (vec3/length (vec3/setFromArray v mat 4)))
            scaleZ (/ 1 (vec3/length (vec3/setFromArray v mat 8)))]
        (aset ret  0 (* (aget mat  0) scaleX))
        (aset ret  1 (* (aget mat  1) scaleX))
        (aset ret  2 (* (aget mat  2) scaleX))

        (aset ret  4 (* (aget mat  4) scaleY))
        (aset ret  5 (* (aget mat  5) scaleY))
        (aset ret  6 (* (aget mat  6) scaleY))

        (aset ret  8 (* (aget mat  8) scaleZ))
        (aset ret  9 (* (aget mat  9) scaleZ))
        (aset ret 10 (* (aget mat 10) scaleZ))
        ret))))

(defn makeRotationFromEuler
  "@return{mat4}"
  [m euler]
  (let [x euler.x y euler.y z euler.z
        a (m/cos x)
        b (m/sin x)
        c (m/cos y)
        d (m/sin y)
        e (m/cos z)
        f (m/sin z)]
    (assert (= (.-order euler) "XYZ") "other orders have been truncated here")
    (if (= (.-order euler) "XYZ")
      (let [ae (* a e)
            af (* a f)
            be (* b e)
            bf (* b f)]
        (aset m 0 (* c e))
        (aset m 1 (+ af (* be d)))
        (aset m 2 (- bf (* ae d)))
        (aset m 4 (* (- c) f))
        (aset m 5 (- ae (* bf d)))
        (aset m 6 (+ be (* af d)))
        (aset m 8 d)
        (aset m 9 (* (- b) c))
        (aset m 10 (* a c))))
    (aset m 3 0)
    (aset m 7 0)
    (aset m 11 0)
    (aset m 12 0)
    (aset m 13 0)
    (aset m 14 0)
    (aset m 15 1)
    m))

(defn makeRotationFromQuaternion ;;deprecate
  [m q]
  (gq/toRotationMatrix4 q m))

(defn quat->mat4 ;;add optional result-mat arity
  "@param {!quat/Quaternion}
   @return {!Mat4}"
  [q]
  (gq/toRotationMatrix4 (quat/checked-normalize q) (create)))

(defn compose
  "set matrix to composition of position, quaternion, scale"
  [m position q scalev]
  (makeRotationFromQuaternion m q)
  (gmat/scale m (vx scalev) (vy scalev) (vz scalev))
  (setPosition m position)
  m)

(def decompose
  (let [v (vec3/vec3)
        mat (create)]
    (fn [m position q _scale]
      (let [sx (vec3/length (vec3/setFromValues v (aget m 0) (aget m 1) (aget m 2)))
            sy (vec3/length (vec3/setFromValues v (aget m 4) (aget m 5) (aget m 6)))
            sz (vec3/length (vec3/setFromValues v (aget m 8) (aget m 9) (aget m 10)))
            det (determinant m)
            sx (if (< det 0) (- sx) sx)
            sx' (/ 1 sx)
            sy' (/ 1 sy)
            sz' (/ 1 sz)]
        (vec3/setFromValues position (aget m 12) (aget m 13) (aget m 14))
        (copy mat m)
        (aset* mat 0 sx')
        (aset* mat 1 sx')
        (aset* mat 2 sx')

        (aset* mat 4 sy')
        (aset* mat 5 sy')
        (aset* mat 6 sy')

        (aset* mat 8 sz')
        (aset* mat 9 sz')
        (aset* mat 10 sz')

        (quat/setFromRotationMatrix q mat)

        (vec3/setFromValues _scale sx sy sz)

        m))))

(def lookAt
  (let [x (vec3/vec3)
        y (vec3/vec3)
        z (vec3/vec3)]
    (fn [m eye target up] ;eye components are degrees?
      (vec3/sub eye target z)
      (if (zero? (vec3/lengthSq z))
        (vsetz z 1))
      (vec3/checked-normalize z)
      (vec3/cross  up z x)
      (when (zero? (vec3/lengthSq x))
        (if (== 1 (m/abs (vz up)))
          (vsetx+ z 0.0001)
          (vsetz+ z 0.0001))
        (vec3/checked-normalize z)
        (vec3/cross up z x))
      (vec3/checked-normalize x)
      (vec3/cross z x y)
      (aset m 0 (vx x))
      (aset m 1 (vy x))
      (aset m 2 (vz x))

      (aset m 4 (vx y))
      (aset m 5 (vy y))
      (aset m 6 (vz y))

      (aset m 8 (vx z))
      (aset m 9 (vy z))
      (aset m 10 (vz z))
      m)))

(defn makePerspective [m left right top bottom near far]
  (gmat/makeFrustum m left right bottom top near far))

(defn makeOrthographic [m left right top bottom near far]
  (gmat/makeOrtho m left right bottom top near far))

(defn- get-align
  "@private
   @param {!IVector} psize
   @param {!IVector} align
   @return {!IVector}"
  [psize align]
  (let [[pw ph] psize
        [m0 m1] align
        x       (js/Math.round (* m0 pw))
        y       (js/Math.round (* m1 ph))]
    [x y]))

(defn- get-origin
  "@private
   @param {!IVector} size
   @param {!IVector} origin
   @return {!IVector}"
  [size origin]
  (let [[t0 t1] size
        [m0 m1] origin
        x       (js/Math.round (* (- t0) m0))
        y       (js/Math.round (* (- t1) m1))]
    [x y]))

(defn relative-xf ;;only 2D for now, 3D would set z-plane but is meaningless without perspective
  "considers size of parent, size of element, local xform of element,
   desired baseline origin & alignment, calcs final xform relative to
   provided parent
   @param {!IMap} opts ::
     :size  -> [number number] size of element you are transforming
     :psize -> [number number] size of the parent element
     :align -> [number number] or :center,  where in parent to position element
               [0 0] = top left, [1 1] = bottom right etc
     :origin -> [number number] or :center, defines transform origin of element
     :local-xform -> affine array (optional) transform to multiply against
                     before returning
  @return {!(Float64Array|Float32Array)}"
  [opts]
  (let [{:keys [origin align size psize local-xform]} opts]
    (assert (and size (vector? size) (<= 2 (count size)) (every? number? size))
            ":size -> [w h] required to calculate relative transform")
    (assert (and (vector? size) (<= 2 (count psize)) (every? number? psize))
            ":psize -> [w h] required to calculate relative transform")
    (let [m (ident)]
      (if align
        (let [[a b] (get-align psize (if ^boolean (= align :center) [0.5 0.5] align))]
          (pre-translate! m a b)))
      (if origin
        (let [[a b] (get-origin size (if ^boolean (= origin :center) [0.5 0.5] origin))]
          (pre-translate! m a b)))
      (if local-xform
        (mult m local-xform)
        m))))

(def ^:private shuttle
  #js ["matrix3d("  1  ","   3 ","  5 ","  7 ","
                    9  ","  11 "," 13 "," 15 ","
                   17  ","  19 "," 21 "," 23 ","
                   25  ","  27 "," 29 "," 31 ")"])

(defn ->CSS [m]
  (aset shuttle  1 (epsilon (aget m 0)))
  (aset shuttle  3 (epsilon (aget m 1)))
  (aset shuttle  5 (epsilon (aget m 2)))
  (aset shuttle  7 (epsilon (aget m 3)))
  (aset shuttle  9 (epsilon (aget m 4)))
  (aset shuttle 11 (epsilon (aget m 5)))
  (aset shuttle 13 (epsilon (aget m 6)))
  (aset shuttle 15 (epsilon (aget m 7)))
  (aset shuttle 17 (epsilon (aget m 8)))
  (aset shuttle 19 (epsilon (aget m 9)))
  (aset shuttle 21 (epsilon (aget m 10)))
  (aset shuttle 23 (epsilon (aget m 11)))
  (aset shuttle 25 (epsilon (aget m 12))) ;(js/Math.round (/ (* devicePixelRatio val) devicePixelRatio))
  (aset shuttle 27 (epsilon (aget m 13))) ;;(js/Math.round (/ (* devicePixelRatio val) devicePixelRatio))
  (aset shuttle 29 (epsilon (aget m 14)))
  (aset shuttle 31 (epsilon (aget m 15)))
  (.join shuttle ""))
