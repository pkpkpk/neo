(ns neo.object3D-tests
  (:require-macros [neo.macros :refer [set- vx vy vz]])
  (:require [cljs.test :refer-macros [deftest is testing async are]]
            [neo.math :as m :include-macros true :refer [pi -pi]]
            [neo.object3D :as o3D :refer [object3D]]
            [neo.vec.euler :as euler]
            [neo.vec.vec3 :as vec3]
            [neo.vec.mat4 :as mat4]
            [neo.vec.quaternion :as quat]
            [neo.test-helpers :refer [is= roughly= is-euler-roughly=
                                      are-elements-roughly= *eps*]]))

(deftest lookAt-test
  (let [o (o3D/object3D)
        v (vec3/vec3 0 -1 1)]
    (o3D/lookAt o v)
    (is (roughly= 45 (* m/rad2deg (.. o -rotation -x))))))

(deftest children-test
  (let [parent (o3D/object3D)
        child1 (o3D/object3D)
        child2 (o3D/object3D)]
    (is (zero? (alength (.-children parent))))
    (.add-child parent child1)
    (is (= 1 (alength (.-children parent))))
    (is= child1 (first (.-children parent)))
    (is= parent (.-parent (first (.-children parent))))
    (.add-child parent child1)
    (is (= 1 (alength (.-children parent))))
    (is= child1 (first (.-children parent)))
    (.add-child parent child2)
    (is (= 2 (alength (.-children parent))))
    (is= child2 (second (.-children parent)))
    (.remove-child parent child1)
    (is (= 1 (alength (.-children parent))))
    (is= child2 (first (.-children parent)))
    (.remove-child parent child2)
    (is (zero? (alength (.-children parent))))))

(def x 2)
(def y 3)
(def z 4)
(def w 5)

(deftest applyMatrix-test
  (let [a (o3D/object3D)
        mat (mat4/mat4)
        control-pos (vec3/vec3 2 3 4)
        control-quat (quat/quaternion (* 0.5 (m/sqrt 2)) 0 0 (* 0.5 (m/sqrt 2)))]
    (mat4/rotateX mat (/ pi 2))
    (mat4/setPosition mat (vec3/vec3 x y z))
    (o3D/applyMatrix a mat)
    (are-elements-roughly= (.-position a) control-pos)
    (are-elements-roughly= (.-quaternion a) control-quat)))

(deftest applyQuaternion-test
  (let [a (object3D)
        sqrt (* 0.5 (m/sqrt 2))
        q (quat/quaternion 0 sqrt 0 sqrt)
        control (quat/quaternion (/ sqrt 2) (/ sqrt 2) 0 0)]
    (quat/setFromValues (.-quaternion a) 0.25 0.25 0.25 0.25)
    (o3D/applyQuaternion a q)
    (are-elements-roughly= (.-quaternion a) control)))

(deftest setFromAxisAngle-test
  (let [a (object3D)
        axis (vec3/vec3 0 1 0)
        angle pi
        control (euler/euler -pi 0 -pi)
        e (euler/euler)]
    (o3D/setRotationFromAxisAngle a axis angle)
    (euler/setFromQuaternion e (o3D/getWorldQuaternion a (quat/quaternion)))
    (is-euler-roughly= e control)
    (vec3/setFromValues axis 1 0 0)
    (let [angle 0]
      (euler/setFromValues control 0 0 0)
      (o3D/setRotationFromAxisAngle a axis angle)
      (euler/setFromQuaternion e (o3D/getWorldQuaternion a))
      (is-euler-roughly= e control))))

(deftest setRotationFromEuler-test
  (let [a (object3D)
        rotation (euler/euler (/ 45 m/rad2deg) 0 pi)
        control (euler/eclone rotation)
        e (euler/euler)]
    (o3D/setRotationFromEuler a rotation)
    (euler/setFromQuaternion e (o3D/getWorldQuaternion a))
    (is-euler-roughly= e control)))

(deftest setRotationFromMatrix-test
  (let [a (object3D)
        m (mat4/mat4)
        eye (vec3/vec3)
        target (vec3/vec3 0 1 -1)
        up (vec3/vec3 0 1 0)
        e (euler/euler)]
    (mat4/lookAt m eye target up)
    (o3D/setRotationFromMatrix a m)
    (euler/setFromQuaternion e (o3D/getWorldQuaternion a))
    (is (roughly= 45  (* m/rad2deg (.-x e))))))

(deftest setRotationFromQuaternion-test
  (let [a (object3D)
        rotation (quat/setFromEuler (quat/quaternion) (euler/euler pi 0 -pi))
        e (euler/euler)]
    (o3D/setRotationFromQuaternion a rotation)
    (euler/setFromQuaternion e (o3D/getWorldQuaternion a))
    (is-euler-roughly= e (euler/euler pi 0 -pi))))

(deftest rotation-test
  (testing "rotateX"
    (let [a (object3D)
          angle 1.562]
      (o3D/rotateX a angle)
      (is (roughly= (.. a -rotation -x) angle))))
  (testing "rotateY"
    (let [a (object3D)
          angle -0.346]
      (o3D/rotateY a angle)
      (is (roughly= (.. a -rotation -y) angle))))
  (testing "rotateZ"
    (let [a (object3D)
          angle 1]
      (o3D/rotateZ a angle)
      (is (roughly= (.. a -rotation -z) angle)))))

(deftest translation-test
  (testing "translateOnAxis"
    (let [a (object3D)]
      (o3D/translateOnAxis a (vec3/vec3 1 0 0) 1)
      (o3D/translateOnAxis a (vec3/vec3 0 1 0) 1.23)
      (o3D/translateOnAxis a (vec3/vec3 0 0 1) -4.56)
      (are-elements-roughly= (.-position a) (vec3/vec3 1 1.23 -4.56))))
  (testing "translateX"
    (let [a (object3D)
          d 1.234]
      (o3D/translateX a d)
      (is (roughly= (vx (.. a -position)) d))))
  (testing "translateY"
    (let [a (object3D)
          d 1.234]
      (o3D/translateY a d)
      (is (roughly= (vy (.. a -position)) d))))
  (testing "translateZ"
    (let [a (object3D)
          d 1.234]
      (o3D/translateZ a d)
      (is (roughly= (vz (.. a -position)) d)))))

(deftest getWorldPosition-test
  (let [a (object3D)
        b (object3D)
        control-single (vec3/vec3 x y z)
        control-parent (vec3/vec3 x y 0)
        control-child (vec3/vec3 x y 7)
        position (vec3/vec3)]
    (o3D/translateX a x)
    (o3D/translateY a y)
    (o3D/translateZ a z)
    (are-elements-roughly= (o3D/getWorldPosition a position) control-single)
    (o3D/translateZ b 7)
    (o3D/add-child a b)
    (o3D/translateZ a (- z))
    (are-elements-roughly= (o3D/getWorldPosition a position) control-parent)
    (are-elements-roughly= (o3D/getWorldPosition b position) control-child)))

(deftest getWorldDirection-test
  (let [a (object3D)
        control (vec3/vec3 0 (* -0.5 (m/sqrt 2)) (* 0.5 (m/sqrt 2)))
        direction (vec3/vec3)]
    (o3D/lookAt a (vec3/vec3 0 -1 1))
    (o3D/getWorldDirection a direction)
    (are-elements-roughly= direction control)))

(deftest getWorldScale-test
  (let [a (object3D)
        m (mat4/scale (mat4/create) x y z)
        control (vec3/vec3 x y z)]
    (o3D/applyMatrix a m)
    (are-elements-roughly= (o3D/getWorldScale a) control)))

(deftest traverse-test
  (let [a (object3D)
        b (object3D)
        c (object3D)
        d (object3D)
        names #js[]
        control-normal ["parent", "child", "childchild 1", "childchild 2"]
        control-visible ["parent", "child", "childchild 2"]
        control-ancestors ["child" "parent"]]
    (set! (.-name a) "parent")
    (set! (.-name b) "child")
    (set! (.-name c) "childchild 1")
    (set! (.-visible c) false)
    (set! (.-name d) "childchild 2")

    (o3D/add-child b c)
    (o3D/add-child b d)
    (o3D/add-child a b)
    (testing "traverse"
      (o3D/traverse a (fn [o] (.push names (.-name o))))
      (is= (vec names) control-normal))
    (set! (.-length names) 0)
    (testing "traverseVisible"
      (o3D/traverseVisible a (fn [o] (.push names (.-name o))))
      (is= (vec names) control-visible))
    (set! (.-length names) 0)
    (testing "traverseAncestors"
      (o3D/traverseAncestors c (fn [o] (.push names (.-name o))))
      (is= (vec names) control-ancestors))))