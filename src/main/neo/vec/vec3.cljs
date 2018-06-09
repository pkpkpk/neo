(ns neo.vec.vec3
  (:require-macros [neo.macros :refer [vx vy vz vw
                                       vsetx vsety vsetz
                                       vsetx+ vsety+ vsetz+
                                       vsetx- vsety- vsetz-
                                       vsetx* vsety* vsetz*
                                       goog-typedef]])
  (:require [neo.math :as m :include-macros true]
            [goog.vec.Vec3 :as gvec3]))

(goog-define useFloat32 false)

(if ^boolean useFloat32
  (goog-typedef Vec3 "{goog.vec.Vec3.Float32}")
  (goog-typedef Vec3 "{goog.vec.Vec3.Float64}"))

(defn vec3
  ([]
   (if ^boolean useFloat32
     (gvec3/createFloat32FromValues 0 0 0)
     (gvec3/createFloat64FromValues 0 0 0)))
  ([x y z]
   (if ^boolean useFloat32
     (gvec3/createFloat32FromValues x y z)
     (gvec3/createFloat64FromValues x y z))))

(defn vector3 [& args] (apply vec3 args))

(defn ^boolean equals?
  "Returns true if the components of a are equal to the components of b.
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {boolean} True if the vectors are equal, false otherwise."
  [a b]
  (gvec3/equals a b))

(defn createFromValues
  "Creates a new Vec3 initialized with the supplied values.
   @param {!number} x
   @param {!number} y
   @param {!number} z
   @return {!Vec3}"
  [x y z]
  (if ^boolean useFloat32
    (gvec3/createFloat32FromValues x y z)
    (gvec3/createFloat64FromValues x y z)))

(defn setFromValues
  "Initializes the vector with the given values.
   @param {goog.vec.Vec3.AnyType} v :: The vector to receive the values.
   @param {number} x :: value for element at index 0.
   @param {number} y :: value for element at index 1.
   @param {number} z :: value for element at index 2.
   @return {!goog.vec.Vec3.AnyType} mutated v"
  [v x y z] (gvec3/setFromValues v x y z))

(defn createFromArray
  "Creates a new Vec3 initialized with the value from the given array.
   @param {goog.vec.Vec3.Vec3Like} arr :: The source 3 element array.
   @return {!goog.vec.Vec3.Type} The new 3 element array."
  [arr]
  (if ^boolean useFloat32
    (gvec3/createFloat32FromArray arr)
    (gvec3/createFloat64FromArray arr)))

(defn setFromArray
  "Initializes the vector with the given array of values.
   @param {goog.vec.Vec3.AnyType} v :: The vector to receive the values.
   @param {goog.vec.Vec3.AnyType} arr :: The array of values.
   @return {!goog.vec.Vec3.AnyType} mutated v"
  ([v arr](setFromArray v arr 0))
  ([v arr offset]
   (setFromValues v (aget arr offset)
                    (aget arr (inc offset))
                    (aget arr (+ 2 offset)))))

(def copy setFromArray)
(def vclone createFromArray)

(defn add
  "component-wise addition of a and b
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to a"
  ([a b] (gvec3/add a b a))
  ([res a b](gvec3/add a b res)))

(defn sub
  "component-wise subtraction of a and b
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to a"
  ([a b] (gvec3/subtract a b a))
  ([res a b](gvec3/subtract a b res)))

(defn mult
  "component-wise multiplication of a and b
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to a"
  ([a b]
   (vsetx* a (vx b))
   (vsety* a (vy b))
   (vsetz* a (vz b))
   a)
  ([res a b]
   (vsetx res (* (vx a) (vx b)))
   (vsety res (* (vy a) (vy b)))
   (vsetz res (* (vz a) (vz b)))
   res))

(defn div
  "component-wise division of a and b
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to a"
  ([a b]
   (vsetx* a (/ 1 (vx b)))
   (vsety* a (/ 1 (vy b)))
   (vsetz* a (/ 1 (vz b)))
   a)
  ([res a b]
   (vsetx res (/ (vx a) (vx b)))
   (vsety res (/ (vy a) (vy b)))
   (vsetz res (/ (vz a) (vz b)))
   res))

(defn cross
  "Computes the vector (cross) product of a and b
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to a"
  ([a b] (gvec3/cross a b a))
  ([res a b] (gvec3/cross a b res)))

(defn ^number dot
  "Returns the scalar product of vectors a and b.
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {number} The scalar product."
  [a b]
  (gvec3/dot a b))

(defn setScalar
  "Set all components of v to the scalar value
   @param {goog.vec.Vec3.AnyType} v
   @param {number} s :: the scalar value
   @return {!goog.vec.Vec3.AnyType} mutated v"
  [v s]
  (vsetx v s)
  (vsety v s)
  (vsetz v s)
  v)

(defn addScalar
  "Add the scalar value to all components of v
   @param {goog.vec.Vec3.AnyType} v
   @param {number} s :: the scalar value
   @return {!goog.vec.Vec3.AnyType} mutated v"
  [v s]
  (vsetx+ v s)
  (vsety+ v s)
  (vsetz+ v s)
  v)

(defn subScalar
  "Subtract the scalar value to all components of v
   @param {goog.vec.Vec3.AnyType} v
   @param {number} s :: the scalar value
   @return {!goog.vec.Vec3.AnyType} mutated v"
  [v s]
  (vsetx- v s)
  (vsety- v s)
  (vsetz- v s)
  v)

(defn multScalar
  "Multiply each component of v with the scalar value
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} v :: The vector to scale
   @param {number} scalar :: The scalar value
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to v"
  ([v scalar] (gvec3/scale v scalar v))
  ([res v scalar] (gvec3/scale v scalar res)))

(defn divScalar
  "Divide each component of v with the scalar value
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} v :: The vector to scale
   @param {number} scalar :: The scalar value
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to v"
  ([v scalar] (gvec3/scale v (/ 1 scalar) v))
  ([res v scalar] (gvec3/scale v (/ 1 scalar) res)))

(def ^{:doc "alias for multScalar"} scale multScalar)

(defn addScaledVector
  "Add a scaled component in a single call.
   Equivalent to `(addScalar a (multScalar b s))`
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @param {number} scalar :: The scalar value
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to a"
  ([a b s]
   (vsetx+ a (* (vx b) s))
   (vsety+ a (* (vy b) s))
   (vsetz+ a (* (vz b) s))
   a)
  ([res a b s]
   (vsetx res (+ (vx a) (* (vx b) s)))
   (vsety res (+ (vx a) (* (vy b) s)))
   (vsetz res (+ (vx a) (* (vz b) s)))
   res))

(defn negate
  "@param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} v :: The vector to negate.
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to v"
  ([v] (gvec3/negate v v))
  ([res v](gvec3/negate v res)))

(defn ^number length
  "@param {Vec3} v
   @return {number} The magnitude of the vector."
  [v] (gvec3/magnitude v))

(defn ^number lengthSq
  "@param {Vec3} v
   @return {number} The squared magnitude of the vector."
  [v] (gvec3/magnitudeSquared v))

(defn normalize
  "Normalizes the given vector storing the result into resultVec.
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} v :: The vector to normalize.
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to v"
  ([v] (gvec3/normalize v v))
  ([res v] (gvec3/normalize v res)))

(defn checked-normalize
  "Normalizes the given vector storing the result into resultVec.
   The null vec returns itself (instead of NaNs)
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} v :: The vector to normalize.
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to v"
  ([v]
   (if (== 0 (vx v) (vy v) (vz v))
     v
     (gvec3/normalize v v)))
  ([res v]
   (if (== 0 (vx v) (vy v) (vz v))
     v
     (gvec3/normalize v res))))

(defn setLength
  "Set the magnitude of v to a scalar valule
   @param {Vec2} v :: vec to set
   @param {number} len :: desired length
   @return {!Vec2} mutated v"
  [v len]
  (-> v normalize (multScalar len)))

(defn ^number distance
  "Returns the distance between two points.
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {number} The distance between the points."
  [a b] (gvec3/distance a b))

(defn ^number distanceSq
  "Returns the squared distance between two points.
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {number} The squared distance between the points."
  [a b] (gvec3/distanceSquared a b))

(defn ^number manhattanLength
  "@param {Vec3} v
   @return {number} The manhattan-length of the vector."
  [v]
  (+ (m/abs (vx v)) (m/abs (vy v)) (m/abs (vz v))))

(defn ^number manhattanDistance
  "@param {Vec3} v
   @return {number} The manhattan distance between the vectors a & b"
  [a b]
  (+ (m/abs (- (vx a) (vx b)))
     (m/abs (- (vy a) (vy b)))
     (m/abs (- (vz a) (vz b)))))

(defn direction
  "Returns a unit vector pointing from one point to another.
   If the input points are equal then the result will be all zeros.
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to a"
  ([a b] (gvec3/direction a b a))
  ([res a b](gvec3/direction a b res)))

(defn ^number angleTo
  "@param {Vec3} a
   @param {Vec3} b
   @return {number} angle in radians of a to b"
  [a b]
  (let [theta (/ (dot a b) (m/sqrt (* (lengthSq a) (lengthSq b))))]
    (m/acos (goog.math.clamp theta -1 1))))

(def
  ^{:doc
    "reflect incident vector off plane orthogonal to normal
     @param {Vec3} v
     @param {Vec3} normal :: assumed to have unit length
     @return {Vec3}"}
  reflect
  (let [_v (vec3)]
    (fn reflect
      ([v normal](reflect v v normal))
      ([res v normal]
       (sub res v (multScalar (copy _v normal) (* 2 (dot v normal))))))))

(defn lerp
  "Linearly interpolate from a to b according to f. The value of f should be
   in the range [0..1] otherwise the results are undefined.
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} a
   @param {goog.vec.Vec3.AnyType} b
   @param {number} f The interpolation factor.
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to a"
  ([a b f](gvec3/lerp a b f a))
  ([res a b f] (gvec3/lerp a b f res)))

(defn clamp
  "component-wise clamp values between two vectors.
   Assumes components of minv < maxv
   @param {Vec3} v :: vector to clamp
   @param {Vec3} minv :: minimum values
   @param {Vec3} maxv :: maximum values
   @return {Vec3} mutated v"
  [v minv maxv]
  (vsetx v (goog.math.clamp (vx v) (vx minv) (vx maxv)))
  (vsety v (goog.math.clamp (vy v) (vy minv) (vy maxv)))
  (vsetz v (goog.math.clamp (vz v) (vz minv) (vz maxv)))
  v)

(defn clampScalar
  "clamp values between scalar values.
   @param {Vec3} v :: vector to clamp
   @param {number} minVal :: minimum value
   @param {number} maxVal :: maximum value
   @return {Vec3} mutated v"
  [v minVal maxVal]
  (vsetx v (goog.math.clamp (vx v) minVal maxVal))
  (vsety v (goog.math.clamp (vy v) minVal maxVal))
  (vsetz v (goog.math.clamp (vz v) minVal maxVal))
  v)

(defn clampLength
  "clamp the vector's length between scalar values.
   @param {Vec3} v :: vector to clamp
   @param {number} minVal :: minimum value
   @param {number} maxVal :: maximum value
   @return {Vec3} mutated v"
  [v minVal maxVal]
  (let [len (or (length v) 1)]
    (multScalar (divScalar v len) (goog.math.clamp len minVal maxVal))))

(defn vmin
  "component-wise minimum
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} v The source vector.
   @param {goog.vec.Vec3.AnyType|number} limit :: The limit vector or scalar
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to v"
  ([v limit] (gvec3/min v limit v))
  ([res v limit](gvec3/min v limit res)))

(defn vmax
  "component-wise maximum
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} v The source vector.
   @param {goog.vec.Vec3.AnyType|number} limit :: The limit vector or scalar
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to v"
  ([v limit] (gvec3/max v limit v))
  ([res v limit](gvec3/max v limit res)))

(defn abs
  "component-wise absolute values
   @param {goog.vec.Vec3.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec3.AnyType} v
   @return {!goog.vec.Vec3.AnyType} mutated res, defaults to v"
  ([v] (gvec3/abs v v))
  ([result v] (gvec3/abs v result)))

(defn floor
  "component-wise floor
   @param {goog.vec.Vec3.AnyType} v
   @return {!goog.vec.Vec3.AnyType} mutated v"
  [v]
  (vsetx v (m/floor (vx v)))
  (vsety v (m/floor (vy v)))
  (vsetz v (m/floor (vz v)))
  v)

(defn ceil
  "component-wise ceil
   @param {goog.vec.Vec3.AnyType} v
   @return {!goog.vec.Vec3.AnyType} mutated v"
  [v]
  (vsetx v (m/ceil (vx v)))
  (vsety v (m/ceil (vy v)))
  (vsetz v (m/ceil (vz v)))
  v)

(defn round
  "component-wise round
   @param {goog.vec.Vec3.AnyType} v
   @return {!goog.vec.Vec3.AnyType} mutated v"
  [v]
  (vsetx v (m/round (vx v)))
  (vsety v (m/round (vy v)))
  (vsetz v (m/round (vz v)))
  v)

(defn setFromSpherical
  [this s]
  (let [sinPhiRadius (* (m/sin s.phi) s.radius)
        x' (* sinPhiRadius (m/sin s.theta))
        y' (* (m/cos s.phi) s.radius)
        z' (* sinPhiRadius (m/cos s.theta))]
    (vsetx this x')
    (vsety this y')
    (vsetz this z')
    this))

(defn setFromCylindrical [this c]
  (let [x' (* c.radius (m/sin c.theta))
        y' c.y
        z' (* c.radius (m/cos c.theta))]
    (vsetx this x')
    (vsety this y')
    (vsetz this z')
    this))

(defn applyQuaternion [this q]
  (let [x (vx this)
        y (vy this)
        z (vz this)
        qx (vx q)
        qy (vy q)
        qz (vz q)
        qw (vw q)
        ix (+ (* qw x) (* qy z) (- (* qz y)))
        iy (+ (* qw y) (* qz x) (- (* qx z)))
        iz (+ (* qw z) (* qx y) (- (* qy x)))
        iw (+ (- (* qx x)) (-  (* qy y)) (- (* qz z)))
        x' (+ (* qw ix) (* iw (- qx)) (* iy (- qz)) (- (* iz (- qy))))
        y' (+ (* iy qw) (* iw (- qy)) (* iz (- qx)) (- (* ix (- qz))))
        z' (+ (* iz qw) (* iw (- qz)) (* ix (- qy)) (- (* iy (- qx))))]
    (vsetx this x')
    (vsety this y')
    (vsetz this z')
    this))

(defn setFromMatrixColumn [v mat col-index]
  (setFromArray v mat (* col-index 4)))

(defn setFromMatrixPosition [this m]
  (vsetx this (aget m 12))
  (vsety this (aget m 13))
  (vsetz this (aget m 14))
  this)

(defn setFromMatrixScale [v m]
  (let [sx (length (setFromMatrixColumn v m 0))
        sy (length (setFromMatrixColumn v m 1))
        sz (length (setFromMatrixColumn v m 2))]
    (vsetx v sx)
    (vsety v sy)
    (vsetz v sz)
    v))

(defn applyMat4 [v mat]
  (let [x (vx v) y (vy v) z (vz v)
        w (/ 1 (+ (* x (aget mat 3))
                  (* y (aget mat 7))
                  (* z (aget mat 11))
                  (aget mat 15)))
        x' (* w (+ (* x (aget mat 0))
                   (* y (aget mat 4))
                   (* z (aget mat 8))
                   (aget mat 12)))
        y' (* w (+ (* x (aget mat 1))
                   (* y (aget mat 5))
                   (* z (aget mat 9))
                   (aget mat 13)))
        z' (* w (+ (* x (aget mat 2))
                   (* y (aget mat 6))
                   (* z (aget mat 10))
                   (aget mat 14)))]
    (vsetx v x')
    (vsety v y')
    (vsetz v z')
    v))


