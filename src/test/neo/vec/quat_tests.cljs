(ns neo.vec.quat-tests
  (:require-macros [neo.macros :refer [set- vx vy vz vw vsetx- vsety- vsetz- vsetw-]])
  (:require [cljs.test :refer-macros [deftest is testing async are]]
            [neo.math :as m :include-macros true]
            [neo.vec.euler :as euler]
            [neo.vec.vec3 :as vec3]
            [neo.vec.mat4 :as mat4]
            [neo.vec.quaternion :as quat]
            [neo.test-helpers :refer [is= roughly= elements-roughly= are-elements-roughly= *eps*]]))

(defn log [& args] (.apply js/console.log js/console (into-array args)))

(deftest setFromAxisAngle-test
  (let [zero (quat/quaternion)
        a (quat/setFromAxisAngle (quat/quaternion) (vec3/vec3 1 0 0) 0 )
        b (quat/setFromAxisAngle (quat/quaternion) (vec3/vec3 0 1 0) 0 )
        c (quat/setFromAxisAngle (quat/quaternion) (vec3/vec3 0 0 1) 0 )]
    (are-elements-roughly= zero a)
    (are-elements-roughly= zero b)
    (are-elements-roughly= zero c)
    (let [aa (quat/setFromAxisAngle (quat/quaternion) (vec3/vec3 1 0 0) m/pi)
          bb (quat/setFromAxisAngle (quat/quaternion) (vec3/vec3 1 0 0) m/-pi)]
      (is (not (elements-roughly=  c aa)))
      (is (not (elements-roughly=  c bb)))
      (quat/mult aa bb)
      (is= c aa))))

(deftest setFromEuler-roundtrip-test
  (doseq [v [(vec3/vec3 1 0 0)
             (vec3/vec3 0 1 0)
             (vec3/vec3 0 0 1)]]
    (doseq [o ["XYZ"]] ;<======== [ 'XYZ', 'YXZ', 'ZXY', 'ZYX', 'YZX', 'XZY' ]
      (let [e (euler/euler (vx v) (vy v) (vz v) "XYZ")
            q (quat/setFromEuler (quat/quaternion) e)
            e2 (euler/setFromQuaternion (euler/euler) q)
            angle (vec3/vec3 (.-x e2) (.-y e2) (.-z e2))]
        (is (< (vec3/distance angle v) 0.001))))))

(defn o->e [order] (euler/euler  0.1, -0.3, 0.25,  order))

(defn qSub [a b]
  (let [res (quat/qclone a)]
    (vsetx- res (vx b))
    (vsety- res (vy b))
    (vsetz- res (vz b))
    (vsetw- res (vw b))
    res))

(deftest setFromEuler+setFromRotationMatrix-test
  (doseq [order ["XYZ"]]
    (let [e (o->e order)
          q (quat/setFromEuler (quat/quaternion) e)
          m (mat4/makeRotationFromEuler (mat4/mat4) e)
          q2 (quat/setFromRotationMatrix (quat/quaternion) m)]
      (is (< (quat/length (qSub q q2)) 0.001)))))

(deftest setFromRotationMatrix-test
  (let [a (quat/quaternion)
        q (quat/normalize (quat/quaternion  -9, -2, 3, -4))
        m (mat4/makeRotationFromQuaternion (mat4/mat4) q)
        control (quat/quaternion 0.8581163303210332, 0.19069251784911848, -0.2860387767736777, 0.38138503569823695 )]
    (quat/setFromRotationMatrix a m)
    (is (< (m/abs (- (vx a) (vx control))) *eps*))
    (is (< (m/abs (- (vy a) (vy control))) *eps*))
    (is (< (m/abs (- (vz a) (vz control))) *eps*))
    (is (< (m/abs (- (vw a) (vw control))) *eps*)))
  (let [a (quat/quaternion)
        q (quat/normalize (quat/quaternion -1, -2, 1, -1))
        m (mat4/makeRotationFromQuaternion (mat4/mat4) q)
        control (quat/quaternion 0.37796447300922714, 0.7559289460184544, -0.37796447300922714, 0.37796447300922714 )]
    (quat/setFromRotationMatrix a m)
    (is (< (m/abs (- (vx a) (vx control))) *eps*))
    (is (< (m/abs (- (vy a) (vy control))) *eps*))
    (is (< (m/abs (- (vz a) (vz control))) *eps*))
    (is (< (m/abs (- (vw a) (vw control))) *eps*))))

(deftest multiply-test
  (let [angles [(euler/euler 1 0 0) (euler/euler 0 1 0) (euler/euler 0 0 1)]
        q1 (quat/setFromEuler (quat/quaternion) (angles 0))
        q2 (quat/setFromEuler (quat/quaternion) (angles 1))
        q3 (quat/setFromEuler (quat/quaternion) (angles 2))
        m1 (mat4/makeRotationFromEuler (mat4/mat4) (angles 0))
        m2 (mat4/makeRotationFromEuler (mat4/mat4) (angles 1))
        m3 (mat4/makeRotationFromEuler (mat4/mat4) (angles 2))
        q (quat/mult (quat/mult q1 q2 (quat/quaternion)) q3)
        m (mat4/mult (mat4/mult m1 m2) m3)
        qm (quat/setFromRotationMatrix (quat/quaternion) m)]
    (is (< (quat/length (qSub q qm)) 0.001))))