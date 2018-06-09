(ns neo.vec.vec4
  (:require-macros [neo.macros :refer [vx vy vz vw
                                       vsetx vsety vsetz vsetw
                                       vsetx+ vsety+ vsetz+ vsetw+
                                       vsetx- vsety- vsetz- vsetw-
                                       vsetx* vsety* vsetz* vsetw*
                                       goog-typedef]])
  (:require [neo.math :as m :include-macros true]
            [goog.vec.Vec4 :as gvec4]))

(goog-define useFloat32 false)

(if ^boolean useFloat32
  (goog-typedef Vec4 "{goog.vec.Vec4.Float32}")
  (goog-typedef Vec4 "{goog.vec.Vec4.Float64}"))

(defn vec4
  ([]
   (if ^boolean useFloat32
     (gvec4/createFloat32FromValues 0 0 0 0)
     (gvec4/createFloat64FromValues 0 0 0 0)))
  ([x y z w]
   (if ^boolean useFloat32
     (gvec4/createFloat32FromValues x y z w)
     (gvec4/createFloat64FromValues x y z w))))

(defn setFromValues [v x y z w]
  (gvec4/setFromValues v x y z w))

(defn applyMat4 [v m]
  (let [x (vx v) y (vy v) z (vz v) w (vw v)
        x' (+ (* x (aget m 0) ) (* y (aget m 4)) (* z (aget m 8)) (* w (aget m 12)))
        y' (+ (* x (aget m 1) ) (* y (aget m 5)) (* z (aget m 9)) (* w (aget m 13)))
        z' (+ (* x (aget m 2) ) (* y (aget m 6)) (* z (aget m 10)) (* w (aget m 14)))
        w' (+ (* x (aget m 3) ) (* y (aget m 7)) (* z (aget m 11)) (* w (aget m 15)))]
    (vsetx v x')
    (vsety v y')
    (vsetz v z')
    (vsetw v w')
    v))
