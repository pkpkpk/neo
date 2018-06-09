(ns neo.vec.affine
  "This namespace represents 3D affine transform matrices using typed arrays.
   The [0 0 0 1] bottom row is elided, so the (3x4) matrix is stored in arrays of
   length 12 in column-major order.

    | m00  m01 m02 m03 |
    | m10  m11 m12 m13 | => [m00 m10 m20  m01 m11 m21  m02 m12 m22  m03 m13 m23]
    | m20  m21 m22 m23 |
    |   0    0   0   1 |

   Knowing that the bottom row is constant allows for less work vs
   equivalent operations on full 4x4 matrices. In addition, many affine
   transforms are mostly sparse.

   By default, the array type used is Float64Array. If for some reason you need
   Float32Arrays, in your compiler config add:
     {:closure-defines {neo.vec.affine/useFloat32 true}}

   + Bang(!) functions mutate a passed matrix with functions optimized for that
     particular multiplication.
   + Bang-less functions return a new matrix with the given transform.
   + (affine/mult a b) does a full 'affine multiplication' of a & b, returning
     the mutated b matrix

   This is typically what you want:

     (-> (affine/rotateX pi) ;<-- need mat to start with
       (affine/translate! 1 2 3) ; <-- in-place mutation!
       (affine/scale! 4 5 6)
       (affine/shear! 7 8 9)
       affine/->CSS)

   The above is equivalent to, but faster than:

     (-> (affine/rotateX pi)
       (affine/mult (affine/translate 1 2 3)) ; <-- alloc + full mult for each
       (affine/mult (affine/scale 4 5 6))
       (affine/mult (affine/shear 7 8 9))
       affine/->CSS)

   The ->CSS function converts a transform into a string suitable for the
   CSS matrix3d api. See:
   https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/matrix3d"
  (:require-macros [neo.math :refer [sin cos tan round abs]]
                   [neo.vec.compiler :as c]))

(goog-define useFloat32 false)

(defn valid-matrix?
  "@param {!*} m
   @return {!boolean}"
   [m]
   (and (= (type m) (if ^boolean useFloat32 js/Float32Array js/Float64Array))
        (= 12 (alength m))))

(defn create
  "@return {!(Float64Array|Float32Array)} The new matrix, all zeros"
  []
  (if ^boolean useFloat32
    (new js/Float32Array 12)
    (new js/Float64Array 12)))

(defn setFromValues
  "Creates an affine matrix initialized from the given values.
   @param {!(Float64Array|Float32Array)} m :: the matrix to recieve the values
   @param {!number} m00 :: The values at (0, 0).
   @param {!number} m10 :: The values at (1, 0).
   @param {!number} m20 :: The values at (2, 0).
   @param {!number} m01 :: The values at (0, 1).
   @param {!number} m11 :: The values at (1, 1).
   @param {!number} m21 :: The values at (2, 1).
   @param {!number} m02 :: The values at (0, 2).
   @param {!number} m12 :: The values at (1, 2).
   @param {!number} m22 :: The values at (2, 2).
   @param {!number} m03 :: The values at (0, 3).
   @param {!number} m13 :: The values at (1, 3).
   @param {!number} m23 :: The values at (2, 3).
   @return {!(Float64Array|Float32Array)} The new, 16 element array."
   [m m00 m10 m20  m01 m11 m21  m02 m12 m22  m03 m13 m23]
   (aset m 0 m00)
   (aset m 1 m10)
   (aset m 2 m20)
   (aset m 3 m01)
   (aset m 4 m11)
   (aset m 5 m21)
   (aset m 6 m02)
   (aset m 7 m12)
   (aset m 8 m22)
   (aset m 9 m03)
   (aset m 10 m13)
   (aset m 11 m23)
   m)

(defn createFromValues
  "Creates an affine matrix initialized from the given values.
   @param {!number} m00 :: The values at (0, 0).
   @param {!number} m10 :: The values at (1, 0).
   @param {!number} m20 :: The values at (2, 0).
   @param {!number} m01 :: The values at (0, 1).
   @param {!number} m11 :: The values at (1, 1).
   @param {!number} m21 :: The values at (2, 1).
   @param {!number} m02 :: The values at (0, 2).
   @param {!number} m12 :: The values at (1, 2).
   @param {!number} m22 :: The values at (2, 2).
   @param {!number} m03 :: The values at (0, 3).
   @param {!number} m13 :: The values at (1, 3).
   @param {!number} m23 :: The values at (2, 3).
   @return {!(Float64Array|Float32Array)} The new, 16 element array."
  [m00 m10 m20  m01 m11 m21  m02 m12 m22  m03 m13 m23]
  (setFromValues (create) m00 m10 m20  m01 m11 m21  m02 m12 m22  m03 m13 m23))

(defn createFromVec [m]
  "@param {!IVector} m :: vector to convert, bottom row is ignored
   @return {?(Float64Array|Float32Array)}"
  (assert (and (vector? m) (or (= (count m) 16) (= (count m) 12))))
  (if ^boolean (= (count m) 16)
    (let [[m00 m10 m20 _ m01 m11 m21 _  m02 m12 m22 _ m03 m13 m23 _] m]
      (createFromValues m00 m10 m20  m01 m11 m21  m02 m12 m22  m03 m13 m23))
    (if ^boolean (= (count m) 12)
      (apply createFromValues m))))

(defn ->v
  "@param {!(Float64Array|Float32Array)} m
   @return {!IVector}"
  [m]
  (assert (valid-matrix? m))
  [(aget m 0) (aget m  1) (aget m  2) 0
   (aget m 3) (aget m  4) (aget m  5) 0
   (aget m 6) (aget m  7) (aget m  8) 0
   (aget m 9) (aget m 10) (aget m 11) 1])

(defn ^boolean equals?
  "@param {!*} a
   @param {!*} b
   @return {!boolean} test if two affine arrays are equivalent"
  [a b]
  (if (identical? a b)
    true
    (and
     (valid-matrix? a)
     (valid-matrix? b)
     (every? true? (map = (array-seq a) (array-seq b))))))

(defn copy
  "@param {!(Float64Array|Float32Array)} m
   @return {!(Float64Array|Float32Array)} A copy of the given transform."
  [m]
  (assert (valid-matrix? m))
  (let [m00 (aget m 0)  m01 (aget m 3) m02 (aget m 6) m03  (aget m 9)
        m10 (aget m 1)  m11 (aget m 4) m12 (aget m 7) m13  (aget m 10)
        m20 (aget m 2)  m21 (aget m 5) m22 (aget m 8) m23  (aget m 11)]
    (createFromValues m00 m10 m20  m01 m11 m21  m02 m12 m22  m03 m13 m23)))

(defn ident
  "@return {!(Float64Array|Float32Array)} the identity transform."
  []
  (let [mat (create)]
    (aset mat 0 1)
    (aset mat 4 1)
    (aset mat 8 1)
    mat))

(defn identity?
  "@param {!*} m
   @return {!boolean} Whether this transform is the identity transform."
  [m]
  (and
   (valid-matrix? m)
   (= 1 (aget m 0) (aget m 4) (aget m 8))
   (= 0 (aget m 3) (aget m 6) (aget m 9)
        (aget m 1) (aget m 7) (aget m 10)
        (aget m 2) (aget m 5) (aget m 11))))

(defn det
  "Affine matrix determinant assuming bottom row is [0 0 0 1]
   @param {!(Float64Array|Float32Array)} m :: transform
   @return {!number}"
  [m]
  (assert (valid-matrix? m))
  (let [m00 (aget m 0)  m01 (aget m 3) m02 (aget m 6) m03  (aget m 9)
        m10 (aget m 1)  m11 (aget m 4) m12 (aget m 7) m13  (aget m 10)
        m20 (aget m 2)  m21 (aget m 5) m22 (aget m 8) m23  (aget m 11)
        a0 (- (* m00 m11) (* m10 m01))
        a1 (- (* m00 m21) (* m20 m01))
        a3 (- (* m10 m21) (* m20 m11))
        b0 (- (* m02 m13) (* m12 m03))
        b1 (- (* m02 m23) (* m22 m03))
        b2 m02
        b3 (- (* m12 m23) (* m22 m13))
        b4 m12
        b5 m22]
    (+ (*  a0  b5) (- (* a1  b4)) (* a3 b2))))

(defn ^boolean invertible?
  "@param {!(Float64Array|Float32Array)} m :: transform
   @return {!boolean}"
  [m]
  (assert (valid-matrix? m))
  (let [d (det m)]
    (and (js/isFinite d)
         (not (zero? det))
         (js/isFinite (aget m 9))
         (js/isFinite (aget m 10))
         (js/isFinite (aget m 11)))))

(defn invert
  "Compute affine matrix inverse
   If m is not invertible, returns nil.
   @param {!(Float64Array|Float32Array)} m :: transform to invert.
   @return {?IVector} inverted transform or nil"
  [m]
  (assert (valid-matrix? m))
  (let [m00 (aget m 0)  m01 (aget m 3) m02 (aget m 6) m03  (aget m 9)
        m10 (aget m 1)  m11 (aget m 4) m12 (aget m 7) m13  (aget m 10)
        m20 (aget m 2)  m21 (aget m 5) m22 (aget m 8) m23  (aget m 11)
        c0  (- (* m11 m22) (* m21 m12))
        c1  (- (* m01 m22) (* m21 m02))
        c2  (- (* m01 m12) (* m11 m02))
        c4  (- (* m10 m22) (* m20 m12))
        c5  (- (* m00 m22) (* m20 m02))
        c6  (- (* m00 m12) (* m10 m02))
        c8  (- (* m10 m21) (* m20 m11))
        c9  (- (* m00 m21) (* m20 m01))
        c10 (- (* m00 m11) (* m10 m01))
        detM (+ (* m00 c0) (- (* m10 c1)) (*  m20 c2))]
    (when-not (zero? detM)
      (let [invD (/ 1 detM)]
        (createFromValues
         (* invD  c0), (*  (- invD)  c4), (* invD c8)
         (* (- invD) c1), (* invD c5), (* (- invD) c9),
         (* invD  c2), (* (- invD) c6), (* invD c10),
         0 0 0)))))

(defn mult
  "Affine multiplication assuming bottom rows are each [0 0 0 1].
   @param {!(Float64Array|Float32Array)} a
   @param {!(Float64Array|Float32Array)} b
   @return {!(Float64Array|Float32Array)} the mutated b param"
  [a b]
  (c/affine-mult a b))

(defn **
  "Overloadable multiplication function that handles nil, allowing for flexible
   xform composition. If given all nils, returns identity
   @return {!IVector}"
  [& xfs]
  (let [xfs (remove nil? xfs)]
    (if-not (seq xfs)
      (ident)
      (reduce mult xfs))))

(defn translate ;=> translation
  "@return {!(Float64Array|Float32Array)} the translation matrix"
  ([x y]
   (c/->affine-array [1 0 0 0,  0 1 0 0,  0 0 1 0,  x y 0 1]))
  ([x y z]
   (c/->affine-array [1 0 0 0,  0 1 0 0,  0 0 1 0,  x y z 1])))

(defn translate!
  "shortcut for (mult m (translate x y z))
   @return {!(Float64Array|Float32Array)} mutated affine transform."
  ([m x y]
   (c/affine-mult m [1 0 0 0,  0 1 0 0,  0 0 1 0,  x y 0 1]))
  ([m x y z]
   (c/affine-mult m [1 0 0 0,  0 1 0 0,  0 0 1 0,  x y z 1])))

(defn pre-translate!
  "Shortcut for (mult (translate x y z) m)
   @return {!(Float64Array|Float32Array)} mutated affine transform."
  ([m x y]
   (c/affine-mult [1 0 0 0,  0 1 0 0,  0 0 1 0,  x y 0 1] m))
  ([m x y z]
   (c/affine-mult [1 0 0 0,  0 1 0 0,  0 0 1 0,  x y z 1] m)))

(defn get-translation
  "@param {!(Float64Array|Float32Array)} m :: transform
   @return {!IVector} [x y z]"
  [m] [(aget m 9) (aget m 10) (aget m 11)])

(defn move-back!
  "Move a transform back in the stacking context
   @return {!(Float64Array|Float32Array)} mutated affine transform."
  ([m]
   (c/affine-mult m [1 0 0 0   0 1 0 0   0 0 1 0   0 0 -1e-3 1]))
  ([m d]
   (c/affine-mult m [1 0 0 0   0 1 0 0   0 0 1 0   0 0 (* d -1e-3) 1])))

(defn move-up!
  "Move a transform torwards front of the stacking context
   @return {!(Float64Array|Float32Array)} mutated affine transform."
  ([m]
   (c/affine-mult m [1 0 0 0   0 1 0 0   0 0 1 0   0 0  1e-3 1]))
  ([m d]
   (c/affine-mult m [1 0 0 0   0 1 0 0   0 0 1 0   0 0 (* d  1e-3) 1])))

(defn scale
  "A single param will be used for both x + y
   @return {!(Float64Array|Float32Array)} the scale matrix"
  ([sx]
   (c/->affine-array [sx 0 0 0, 0 sx 0 0, 0 0 1 0, 0 0 0 1]))
  ([sx sy]
   (c/->affine-array [sx 0 0 0, 0 sy 0 0, 0 0 1 0, 0 0 0 1]))
  ([sx sy sz]
   (c/->affine-array [sx 0 0 0, 0 sy 0 0, 0 0 sz 0, 0 0 0 1])))

(defn get-scale
  "@param {!(Float64Array|Float32Array)} m :: transform
   @return {!IVector} [x y z]"
  [m] [(aget m 0) (aget m 4) (aget m 8)])

(defn scale!
  "Shortcut for (mult m (scale x ?y ?z))
   - A single param will be used for both x + y
   @return {!(Float64Array|Float32Array)} mutated affine transform."
  ([m sx]
   (c/affine-mult m [sx 0 0 0, 0 sx 0 0, 0 0 1 0, 0 0 0 1]))
  ([m sx sy]
   (c/affine-mult m [sx 0 0 0, 0 sy 0 0, 0 0 1 0, 0 0 0 1]))
  ([m sx sy sz]
   (c/affine-mult m [sx 0 0 0, 0 sy 0 0, 0 0 sz 0, 0 0 0 1])))

(defn pre-scale!
  "Shortcut for (mult (scale x ?y ?z) m)
   - A single param will be used for both x + y
   @return {!(Float64Array|Float32Array)} This affine transform."
  ([m sx]
   (c/affine-mult [sx 0 0 0, 0 sx 0 0, 0 0 1 0, 0 0 0 1] m))
  ([m sx sy]
   (c/affine-mult [sx 0 0 0, 0 sy 0 0, 0 0 1 0, 0 0 0 1] m))
  ([m sx sy sz]
   (c/affine-mult [sx 0 0 0, 0 sy 0 0, 0 0 sz 0, 0 0 0 1] m)))

(defn rotateX
  "@param {!Number} theta :: angle in radians
   @return {!(Float64Array|Float32Array)} a rotation about the X axis"
  [theta]
  (let [cosTheta (cos theta)
        sinTheta (sin theta)
        -sinTheta (- sinTheta)]
    (createFromValues 1 0 0 0 cosTheta sinTheta 0 -sinTheta cosTheta 0 0 0)))

(defn rotateX!
  "Rotate the given matrix by angle about the x axis.  Equivalent to:
     (mult m (rotateX theta))
   @param {!(Float64Array|Float32Array)} m :: the matrix to mutate
   @param {!number} theta :: angle in radians
   @return {!(Float64Array|Float32Array)} the mutated matrix"
  [m theta]
  (c/affine-mult m [1 0 0 0, 0 (cos theta) (sin theta) 0, 0 (- (sin theta)) (cos theta) 0, 0 0 0 1]))

(defn rotateY
  "@param {!Number} theta :: angle in radians
   @return {!(Float64Array|Float32Array)} a rotation about the Y axis"
  [theta]
  (let [cosTheta (cos theta)
        sinTheta (sin theta)
        -sinTheta (- sinTheta)]
    (createFromValues cosTheta 0 -sinTheta 0 1 0 sinTheta 0 cosTheta 0 0 0)))

(defn rotateY!
  "Rotate the given matrix by angle about the y axis.  Equivalent to:
     (mult m (rotateY theta))
   @param {!(Float64Array|Float32Array)} m :: the matrix to mutate
   @param {!Number} theta :: angle in radians
   @return {!(Float64Array|Float32Array)} the mutated matrix"
  [m theta]
  (c/affine-mult m [(cos theta) 0 (- (sin theta)) 0,  0 1 0 0,  (sin theta) 0 (cos theta) 0, 0 0 0 1]))

(defn rotateZ
  "@param {!Number} theta :: angle in radians
   @return {!(Float64Array|Float32Array)} a rotation about the Z axis"
  [theta]
  (let [cosTheta (cos theta)
        sinTheta (sin theta)
        -sinTheta (- sinTheta)]
    (createFromValues cosTheta sinTheta 0 -sinTheta cosTheta 0 0 0 1 0 0 0)))

(defn rotateZ!
  "Rotate the given matrix by angle about the z axis. Equivalent to:
     (mult m (rotateZ theta))
   @param {!(Float64Array|Float32Array)} m :: the matrix to mutate
   @param {!Number} theta :: angle in radians
   @return {!(Float64Array|Float32Array)} the mutated matrix"
  [m theta]
  (c/affine-mult m [(cos theta) (sin theta) 0 0 (- (sin theta)) (cos theta) 0 0 0 0 1 0 0 0 0 1]))

(defn rotateXYZ
  "Equivalent to mult(rotateX(phi), rotateY(theta), rotateZ(psi))
   @param {!Vector} v :: a 3-tuple of [phi theta psi] numbers
     - phi = radians to rotate about the positive x axis
     - theta = radians to rotate about the positive y axis
     - psi = radians to rotate about the positive z axis
   @return {!(Float64Array|Float32Array)} Clockwise rotation about each axis"
  [v]
  (let [[phi theta psi] v
        sinPhi (sin phi)
        sinTheta (sin theta)
        sinPsi (sin psi)
        cosPhi (cos phi)
        cosTheta (cos theta)
        cosPsi (cos psi)]
    (createFromValues
     (* cosTheta cosPsi)
     (+ (* cosPhi sinPsi) (* sinPhi  sinTheta cosPsi))
     (- (* sinPhi sinPsi) (* cosPhi sinTheta cosPsi))

     (* (- cosTheta) sinPsi)
     (- (* cosPhi cosPsi) (* sinPhi sinTheta sinPsi))
     (+ (* sinPhi cosPsi) (* cosPhi sinTheta sinPsi))

     sinTheta
     (* (- sinPhi) cosTheta)
     (* cosPhi cosTheta)

     0 0 0)))

(defn rotate-axis
  "Makes a rotation matrix with the given rotation angle about the axis
   defined by the vector (ax, ay, az).
   @param {!IVector} v :: unit vector representing the axis to rotate about
   @param {!Number} theta :: radians to rotate clockwise about the axis
   @return {!(Float64Array|Float32Array)} transform for axis-angle rotation"
  [theta v]
  (let [[ax ay az] v
        cosTheta (cos theta)
        versineTheta (- 1 cosTheta)
        sinTheta (sin theta)]
    (createFromValues
     (+ cosTheta (* ax ax versineTheta))
     (+ (* ax ay versineTheta) (* az sinTheta))
     (- (* ax az versineTheta) (* ay sinTheta))

     (- (* ax ay versineTheta) (* az sinTheta))
     (+ cosTheta (* ay ay versineTheta))
     (+ (* ay az versineTheta) (* ax sinTheta))

     (+ (* ax az versineTheta) (* ay sinTheta))
     (- (* ay az versineTheta) (* ax sinTheta))
     (+ cosTheta (* az az versineTheta))

     0 0 0)))

(defn rotate-axis!
  "Rotate the given matrix by angle about the x,y,z axis. Equivalent to:
     (mult m (rotate-axis  theta [x y z]))
   @param {!(Float64Array|Float32Array)} m :: matrix to rotate
   @param {!Number} theta :: radians to rotate clockwise about the axis
   @param {!IVector} v :: unit vector [x y z] representing the axis to rotate about
   @return {!(Float64Array|Float32Array)} the rotated matrix"
  [m theta v]
  (let [[x y z] v
        sinTheta (sin theta)
        cosTheta (cos theta)
        versineTheta (- 1 cosTheta)
        xxV (* x x versineTheta)
        xyV (* x y versineTheta)
        xzV (* x z versineTheta)
        yyV (* y y versineTheta)
        yzV (* y z versineTheta)
        zzV (* z z versineTheta)
        xs (* x sinTheta)
        ys (* y sinTheta)
        zs (* z sinTheta)]
    (c/affine-mult m
                    [(+ cosTheta xxV)
                     (+ xyV zs)
                     (- xzV ys)
                     0

                     (- xyV zs)
                     (+ cosTheta yyV)
                     (+ yzV xs)
                     0

                     (+ xzV ys)
                     (- yzV xs)
                     (+ cosTheta zzV)
                     0

                     0 0 0 1])))

(defn pre-rotate-axis!
  "pre-concatenate the given matrix with a rotation matrix by angle about the
   x,y,z axis. Equivalent to:
      (mult (rotate-axis  theta [x y z]) m)
   @param {!(Float64Array|Float32Array)} m :: matrix to pre-concatenate
   @param {!Number} theta :: radians to rotate clockwise about the axis
   @param {!IVector} v :: unit vector [x y z] representing the axis to rotate about
   @return {!(Float64Array|Float32Array)} the rotated matrix"
  [m theta v]
  (let [[x y z] v
        sinTheta (sin theta)
        cosTheta (cos theta)
        versineTheta (- 1 cosTheta)
        xxV (* x x versineTheta)
        xyV (* x y versineTheta)
        xzV (* x z versineTheta)
        yyV (* y y versineTheta)
        yzV (* y z versineTheta)
        zzV (* z z versineTheta)
        xs (* x sinTheta)
        ys (* y sinTheta)
        zs (* z sinTheta)]
    (c/affine-mult
     [(+ cosTheta xxV)
      (+ xyV zs)
      (- xzV ys)
      0

      (- xyV zs)
      (+ cosTheta yyV)
      (+ yzV xs)
      0

      (+ xzV ys)
      (- yzV xs)
      (+ cosTheta zzV)
      0

      0 0 0 1]
     m)))

(defn shear
  "@return {!(Float64Array|Float32Array)} the shear matrix"
  ([psi]
   (c/->affine-array [1 0 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1]))
  ([psi theta]
   (c/->affine-array [1 (tan theta) 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1]))
  ([psi theta phi]
   (c/->affine-array [1 (tan theta) 0 0, (tan psi) 1 0, 0 0 (tan phi) 1 0, 0 0 0 1])))

(defn shear!
  "shortcut for (mult m (shear phi theta psi))
   @return {!(Float64Array|Float32Array)}"
  ([m psi]
   (c/affine-mult m [1 0 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1]))
  ([m psi theta]
   (c/affine-mult m [1 (tan theta) 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1]))
  ([m psi theta phi]
   (c/affine-mult m [1 (tan theta) 0 0, (tan psi) 1 0, 0 0 (tan phi) 1 0, 0 0 0 1])))

(defn pre-shear!
  "shortcut for (mult (shear phi theta psi) m)
   @return {!(Float64Array|Float32Array)}"
  ([m psi]
   (c/affine-mult [1 0 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1] m))
  ([m psi theta]
   (c/affine-mult [1 (tan theta) 0 0, (tan psi) 1 0 0, 0 0 1 0, 0 0 0 1] m))
  ([m psi theta phi]
   (c/affine-mult [1 (tan theta) 0 0, (tan psi) 1 0, 0 0 (tan phi) 1 0, 0 0 0 1] m)))

(defn shear-x
  "@param {!Number} angle :: radians between the top and left sides
   @return {!(Float64Array|Float32Array)}"
  [angle]
  (c/->affine-array [1 0 0 0 (tan angle) 1 0 0 0 0 1 0 0 0 0 1]))

(defn shear-x!
  "@param {!(Float64Array|Float32Array)} m :: transform
   @param {!Number} angle :: radians between the top and left sides
   @return {!(Float64Array|Float32Array)}"
  [m angle]
  (c/affine-mult m [1 0 0 0 (tan angle) 1 0 0 0 0 1 0 0 0 0 1]))

(defn shear-y
  "@param {!Number} angle :: radians between the top and right sides
   @return {!(Float64Array|Float32Array)}"
  [angle]
  (c/->affine-array [1 (tan angle) 0 0  0 1 0 0 0 0 1 0 0 0 0 1]))

(defn shear-y!
  "@param {!(Float64Array|Float32Array)} m :: transform
   @param {!Number} angle :: radians between the top and right sides
   @return {!(Float64Array|Float32Array)}"
  [m angle]
  (c/affine-mult m [1 (tan angle) 0 0  0 1 0 0 0 0 1 0 0 0 0 1]))

(defn- get-align
  "@private
   @param {!IVector} psize
   @param {!IVector} align
   @return {!IVector}"
  [psize align]
  (let [[pw ph] psize ;3D support?
        [m0 m1] align
        x       (round (* m0 pw))
        y       (round (* m1 ph))]
    [x y]))

(defn- get-origin
  "@private
   @param {!IVector} size
   @param {!IVector} origin
   @return {!IVector}"
  [size origin]
  (let [[t0 t1] size
        [m0 m1] origin
        x       (round (* (- t0) m0))
        y       (round (* (- t1) m1))]
    [x y]))

(defn relative-xf
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
    (assert (and size (vector? size) (= 2 (count size)) (every? number? size))
            ":size -> [w h] required to calculate relative transform")
    (assert (and (vector? size) (= 2 (count psize)) (every? number? psize))
            ":psize -> [w h] required to calculate relative transform")
    (when local-xform (assert (valid-matrix? local-xform)))
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

(def ^:const eps 1e-6)

(defn- epsilon
  "@param {!number} n
   @return {!number} rounds |n| smaller than epsilon to 0"
  [n]
  (if ^boolean (< (abs n) eps)
    0
    n))

(def shuttle
  #js ["matrix3d(" 1  ","   3 ","  5 ",0,"
                   7  ","   9 "," 11 ",0,"
                  13  ","  15 "," 17 ",0,"
                  19  ","  21 "," 23 ",1)"])

(defn ->CSS
  "@param {!(Float64Array|Float32Array)} m
   @return {!string} 'matrix3d(<m00>, ... , 1)'"
  [m]
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
  (.join shuttle ""))
