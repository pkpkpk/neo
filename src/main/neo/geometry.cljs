(ns neo.geometry
  (:require-macros [neo.macros :refer [vx vy vz]])
  (:require [neo.math :as m :include-macros true]
            [neo.vec.vec3 :as vec3]))

(def ^:const EPS 0.000001)

(deftype Spherical [radius phi theta]
  Object
  (set [this radius phi theta]
       (set! this.radius radius)
       (set! this.phi phi)
       (set! this.theta theta)
       this)
  (clone [this] (new (.-constructor this) radius phi theta))
  (copy [this that]
        (set! this.radius that.radius)
        (set! this.phi that.phi)
        (set! this.theta that.theta)
        this)
  (makeSafe [this]
    (set! this.phi (max EPS (min phi (- m/pi EPS))))
    this)
  (setFromVector3 [this v]
    (set! this.radius (vec3/length v))
    (if (zero? this.radius)
      (do
        (set! this.theta 0)
        (set! this.phi 0))
      (do
        (set! this.theta (m/atan2 (vx v) (vz v)))
        (set! this.phi (m/acos (goog.math.clamp (/ (vy v) this.radius) -1 1)))))
    this))

(defn spherical
  ([](Spherical. 1.0 0 0))
  ([radius phi theta](Spherical. radius phi theta)))


(deftype Cylindrical [radius theta y]
  Object
  (set [this radius theta y]
       (set! this.radius radius)
       (set! this.theta theta)
       (set! this.y y)
       this)
  (clone [this] (new (.-constructor this) radius theta y))
  (copy [this that]
        (set! this.radius that.radius)
        (set! this.theta that.theta)
        (set! this.y that.y)
        this)
  (setFromVector3 [this v]
                  (set! this.radius (m/sqrt (+ (* (vx v) (vx v)) (* (vz v) (vz v)))))
                  (set! this.theta (m/atan2 (vx v) (vz v)))
                  (set! this.y (vy v))
                  this))

(defn cylindrical
  ([](Cylindrical. 1.0 0 0))
  ([radius theta y](Cylindrical. radius theta y)))