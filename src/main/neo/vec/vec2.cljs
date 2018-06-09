(ns neo.vec.vec2
  (:require-macros [neo.macros :refer [vx vy vsetx vsety
                                       vsetx+ vsety+
                                       vsetx- vsety- vsetx* vsety*
                                       goog-typedef]])
  (:require [neo.math :as m :include-macros true]
            [goog.vec.Vec2 :as gvec2]))

(goog-define useFloat32 false)

(if ^boolean useFloat32
  (goog-typedef Vec2 "{goog.vec.Vec2.Float32}")
  (goog-typedef Vec2 "{goog.vec.Vec2.Float64}"))

(defn vec2
  ([]
   (if ^boolean useFloat32
     (gvec2/createFloat32FromValues 0 0 0)
     (gvec2/createFloat64FromValues 0 0 0)))
  ([x y]
   (if ^boolean useFloat32
     (gvec2/createFloat32FromValues x y)
     (gvec2/createFloat64FromValues x y))))

(defn vector2 [& args] (apply vec2 args))

(defn ^boolean equals?
  "Returns true if the components of a are equal to the components of b.
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {boolean} True if the vectors are equal, false otherwise."
  [a b]
  (gvec2/equals a b))

(defn createFromValues
  "Creates a new Vec2 initialized with the supplied values.
   @param {number} x
   @param {number} y
   @return {!Vec2}"
  [x y]
  (if ^boolean useFloat32
    (gvec2/createFloat32FromValues x y)
    (gvec2/createFloat64FromValues x y)))

(defn setFromValues
  "Initializes the vector with the given values.
   @param {goog.vec.Vec2.AnyType} v :: The vector to receive the values.
   @param {number} x :: value for element at index 0.
   @param {number} y :: value for element at index 1.
   @return {!goog.vec.Vec2.AnyType} mutated v"
  [v x y] (gvec2/setFromValues v x y))

(defn createFromArray
  "Creates a new Vec2 initialized with the value from the given array.
   @param {goog.vec.Vec2.Vec2Like} arr :: The source 3 element array.
   @return {!goog.vec.Vec2.Type} The new 3 element array."
  [arr]
  (if ^boolean useFloat32
    (gvec2/createFloat32FromArray arr)
    (gvec2/createFloat64FromArray arr)))

(defn setFromArray
  "Initializes the vector with the given array of values.
   @param {goog.vec.Vec2.AnyType} v :: The vector to receive the values.
   @param {goog.vec.Vec2.AnyType} arr :: The array of values.
   @return {!goog.vec.Vec2.AnyType} mutated v"
  ([v arr] (gvec2/setFromArray v arr))
  ([v arr offset] (setFromValues v (aget arr offset) (aget arr (inc offset)))))

(def copy setFromArray)
(def vclone createFromArray)

(defn add
  "component-wise addition of a and b
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to a"
  ([a b] (gvec2/add a b a))
  ([res a b](gvec2/add a b res)))

(defn sub
  "component-wise subtraction of a and b
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to a"
  ([a b] (gvec2/subtract a b a))
  ([res a b](gvec2/subtract a b res)))

(defn mult
  "component-wise multiplication of a and b
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to a"
  ([a b]
   (vsetx* a (vx b))
   (vsety* a (vy b))
   a)
  ([res a b]
   (vsetx res (* (vx a) (vx b)))
   (vsety res (* (vy a) (vy b)))
   res))

(defn div
  "component-wise division of a and b
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to a"
  ([a b]
   (vsetx* a (/ 1 (vx b)))
   (vsety* a (/ 1 (vy b)))
   a)
  ([res a b]
   (vsetx res (/ (vx a) (vx b)))
   (vsety res (/ (vy a) (vy b)))
   res))

(defn ^number dot
  "Returns the scalar product of vectors a and b.
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {number} The scalar product."
  [a b]
  (gvec2/dot a b))

(defn setScalar
  "set all components of v to the scalar value
   @param {goog.vec.Vec2.AnyType} v
   @param {number} s :: the scalar value
   @return {!goog.vec.Vec2.AnyType} mutated v"
  [v s]
  (vsetx v s)
  (vsety v s)
  v)

(defn addScalar
  "Add the scalar value to all components of v
   @param {goog.vec.Vec.AnyType} v
   @param {number} s :: the scalar value
   @return {!goog.vec.Vec2.AnyType} mutated v"
  [v s]
  (vsetx+ v s)
  (vsety+ v s)
  v)

(defn subScalar
  "Subtract the scalar value to all components of v
   @param {goog.vec.Vec2.AnyType} v
   @param {number} s :: the scalar value
   @return {!goog.vec.Vec2.AnyType} mutated v"
  [v s]
  (vsetx- v s)
  (vsety- v s)
  v)

(defn multScalar
  "Multiply each component of v with the scalar value
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} v :: The vector to scale
   @param {number} scalar :: The scalar value
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to v"
  ([v scalar] (gvec2/scale v scalar v))
  ([res v scalar] (gvec2/scale v scalar res)))

(defn divScalar
  "Divide each component of v with the scalar value
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} v :: The vector to scale
   @param {number} scalar :: The scalar value
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to v"
  ([v scalar] (multScalar v (/ 1 scalar)))
  ([res v scalar] (multScalar res v (/ 1 scalar))))

(def ^{:doc "alias for multScalar"} scale multScalar)

(defn addScaledVector
  "Add a scaled component in a single call.
   Equivalent to `(addScalar a (multScalar b s))`
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @param {number} scalar :: The scalar value
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to a"
  ([a b s]
   (vsetx+ a (* (vx b) s))
   (vsety+ a (* (vy b) s))
   a)
  ([res a b s]
   (vsetx res (+ (vx a) (* (vx b) s)))
   (vsety res (+ (vx a) (* (vy b) s)))
   res))

(defn negate
  "@param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} v :: The vector to negate.
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to v"
  ([v] (gvec2/negate v v))
  ([res v](gvec2/negate v res)))

(defn ^number length
  "@param {Vec2} v
   @return {number} The magnitude of the vector."
  [v] (gvec2/magnitude v))

(defn ^number lengthSq
  "@param {Vec2} v
   @return {number} The squared magnitude of the vector."
  [v] (gvec2/magnitudeSquared v))

(defn normalize
  "Normalizes the given vector storing the result into resultVec.
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} v :: The vector to normalize.
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to v"
  ([v] (gvec2/normalize v v))
  ([res v] (gvec2/normalize v res)))

(defn checked-normalize
  "Normalizes the given vector storing the result into resultVec.
   The null vec returns itself (instead of NaNs)
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} v :: The vector to normalize.
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to v"
  ([v]
   (if (== 0 (vx v) (vy v))
     v
     (gvec2/normalize v v)))
  ([res v]
   (if (== 0 (vx v) (vy v))
     v
     (gvec2/normalize v res))))

(defn setLength
  "Set the magnitude of v to a scalar valule
   @param {Vec2} v :: vec to set
   @param {number} len :: desired length
   @return {!Vec2} mutated v"
  [v len]
  (-> v normalize (multScalar len)))

(defn ^number distance
  "Returns the distance between two points.
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {number} The distance between the points."
  [a b] (gvec2/distance a b))

(defn ^number distanceSq
  "Returns the squared distance between two points.
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {number} The squared distance between the points."
  [a b] (gvec2/distanceSquared a b))

(defn ^number manhattanLength
  "@param {Vec2} v
   @return {number} The manhattan-length of the vector."
  [v]
  (+ (m/abs (vx v)) (m/abs (vy v))))

(defn ^number manhattanDistance
  "@param {Vec2} v
   @return {number} The manhattan distance between the vectors a & b"
  [a b]
  (+ (m/abs (- (vx a) (vx b)))
     (m/abs (- (vy a) (vy b)))))

(defn direction
  "Returns a unit vector pointing from one point to another.
   If the input points are equal then the result will be all zeros.
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to a"
  ([a b] (gvec2/direction a b a))
  ([res a b](gvec2/direction a b res)))

(defn rotate
  "Rotate one point around another
   @param {Vec2} v :: point to rotate
   @param {goog.vec.AnyType} point :: point to rotate around
   @param {number} angle :: radians to rotate"
  [v point angle]
  (let [c (m/cos angle)
        s (m/sin angle)
        x (- (vx v) (vx point))
        y (- (vy v) (vy point))]
    (vsetx v (+ (* x c) (- (* y s)) (vx point)))
    (vsety v (+ (* x s) (* y c) (vy point)))
    v))

(defn ^number angle
  "@param {Vec2} v
   @return {number} angle in radians with respect to the positive x-axis"
  [v]
  (let [a (m/atan2 (vy v) (vx v))]
    (if (< a 0)
      (+ a (* 2 m/pi))
      a)))

(defn lerp
  "Linearly interpolate from a to b according to f. The value of f should be
   in the range [0..1] otherwise the results are undefined.
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} a
   @param {goog.vec.Vec2.AnyType} b
   @param {number} f The interpolation factor.
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to a"
  ([a b f](gvec2/lerp a b f a))
  ([res a b f] (gvec2/lerp a b f res)))

(defn clamp
  "component-wise clamp values between two vectors.
   Assumes components of minv < maxv
   @param {Vec2} v :: vector to clamp
   @param {Vec2} minv :: minimum values
   @param {Vec2} maxv :: maximum values
   @return {!Vec2} mutated v"
  [v minv maxv]
  (vsetx v (goog.math.clamp (vx v) (vx minv) (vx maxv)))
  (vsety v (goog.math.clamp (vy v) (vy minv) (vy maxv)))
  v)

(defn clampScalar
  "clamp values between scalar values.
   @param {Vec2} v :: vector to clamp
   @param {number} minVal :: minimum value
   @param {number} maxVal :: maximum value
   @return {!Vec2} mutated v"
  [v minVal maxVal]
  (vsetx v (goog.math.clamp (vx v) minVal maxVal))
  (vsety v (goog.math.clamp (vy v) minVal maxVal))
  v)

(defn clampLength
  "clamp the vector's length between scalar values.
   @param {Vec2} v :: vector to clamp
   @param {number} minVal :: minimum value
   @param {number} maxVal :: maximum value
   @return {Vec2} mutated v"
  [v minVal maxVal]
  (let [len (or (length v) 1)]
    (multScalar (divScalar v len) (goog.math.clamp len minVal maxVal))))

(defn vmin
  "component-wise minimum
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} v The source vector.
   @param {goog.vec.Vec2.AnyType|number} limit :: The limit vector or scalar
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to v"
  ([v limit] (gvec2/min v limit v))
  ([res v limit](gvec2/min v limit res)))

(defn vmax
  "component-wise maximum
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} v The source vector.
   @param {goog.vec.Vec2.AnyType|number} limit :: The limit vector or scalar
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to v"
  ([v limit] (gvec2/max v limit v))
  ([res v limit](gvec2/max v limit res)))

(defn abs
  "component-wise absolute values
   @param {goog.vec.Vec2.AnyType} res :: optional vec to store results in
   @param {goog.vec.Vec2.AnyType} v
   @return {!goog.vec.Vec2.AnyType} mutated res, defaults to v"
  ([v] (gvec2/abs v v))
  ([result v] (gvec2/abs v result)))

(defn floor
  "component-wise floor
   @param {goog.vec.Vec2.AnyType} v
   @return {!goog.vec.Vec2.AnyType} mutated v"
  [v]
  (vsetx v (m/floor (vx v)))
  (vsety v (m/floor (vy v)))
  v)

(defn ceil
  "component-wise ceil
   @param {goog.vec.Vec2.AnyType} v
   @return {!goog.vec.Vec2.AnyType} mutated v"
  [v]
  (vsetx v (m/ceil (vx v)))
  (vsety v (m/ceil (vy v)))
  v)

(defn round
  "component-wise round
   @param {goog.vec.Vec2.AnyType} v
   @return {!goog.vec.Vec2.AnyType} mutated v"
  [v]
  (vsetx v (m/round (vx v)))
  (vsety v (m/round (vy v)))
  v)


