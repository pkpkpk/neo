(ns neo.cam-tests
  (:require-macros [neo.macros :refer [set- vx vy vz]])
  (:require [cljs.test :refer-macros [deftest is testing async are]]
            [neo.math :as m :include-macros true]
            [neo.camera :as cam]
            [neo.object3D :as o3D]
            [neo.vec.vec3 :as vec3]
            [neo.vec.mat4 :as mat4]
            [neo.test-helpers :refer [is= roughly= are-elements-roughly= ]]))

(deftest lookAt-test
  (let [cam (cam/perspective-camera)
        v (vec3/vec3 0  1 -1)]
    (.lookAt cam v)
    (is (roughly= 45 (* m/rad2deg (.. cam -rotation -x))))))

(deftest updateProjectionMatrix-test
  (let [control (vector 0.7330642938613892, 0, 0, 0
                        0, 1.3032253980636597, 0, 0
                        0, 0, -1.000666856765747, -1
                        0, 0, -0.2000666856765747, 0)
        ;; updateProjectionMatrix is called in contructor
        cam (cam/perspective-camera 75 (/ 16 9) 0.1 300)]
    (are-elements-roughly= (.-projectionMatrix cam) control)))