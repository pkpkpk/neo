(ns neo.vec.euler-tests
  (:require [cljs.test :refer-macros [deftest is testing async are]]
            [neo.math :as m :include-macros true]
            [neo.vec.euler :as euler]
            [neo.vec.mat4 :as mat4]
            [neo.vec.quaternion :as quat]
            [neo.test-helpers :refer [is= roughly= are-elements-roughly=]]))

(defn log [& args] (.apply js/console.log js/console (into-array args)))

; (deftest equiv (is= (euler/euler 0 0 0 "XYZ") (euler/euler 0 0 0 "XYZ")))

; const eulerZero = new Euler( 0, 0, 0, "XYZ" );
; const eulerAxyz = new Euler( 1, 0, 0, "XYZ" );
; const eulerAzyx = new Euler( 0, 1, 0, "ZYX" );

(def es [(euler/euler 0 0 0 "XYZ") (euler/euler m/pi m/-pi 1 "XYZ")])

(deftest quat-roundtrip-test
  (doseq [e es]
    (let [q (quat/setFromEuler (quat/quaternion) e)
          e2 (euler/setFromQuaternion (euler/euler) q)
          q2 (quat/setFromEuler (quat/quaternion) e2)]
      (are-elements-roughly= q q2))))

(deftest mat-roundtrip-test
  (doseq [e es]
    (let [m (mat4/makeRotationFromEuler (mat4/mat4) e)
          v (euler/setFromRotationMatrix (euler/euler) m (.-order e))
          m2 (mat4/makeRotationFromEuler (mat4/mat4) v)]
      (are-elements-roughly= m m2))))