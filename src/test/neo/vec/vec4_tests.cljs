(ns neo.vec.vec4-tests
  (:require-macros [neo.macros :refer [set- vx vy vz vw]])
  (:require [cljs.test :refer-macros [deftest is testing async are]]
            [neo.math :as m :include-macros true :refer [pi]]
            [neo.vec.euler :as euler]
            [neo.vec.vec2 :as vec2]
            [neo.vec.vec3 :as vec3]
            [neo.vec.vec4 :as vec4]
            [neo.vec.mat4 :as mat4]
            [neo.vec.quaternion :as quat]
            [neo.test-helpers :refer [is= roughly= are-elements-roughly= *eps* elements-roughly=]]))

(def x 2)
(def y 3)
(def z 4)
(def w 5)