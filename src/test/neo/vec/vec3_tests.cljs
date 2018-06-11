(ns neo.vec.vec3-tests
  (:require-macros [neo.macros :refer [set- vx vy vz vw]])
  (:require [cljs.test :refer-macros [deftest is testing async are]]
            [neo.math :as m :include-macros true :refer [pi]]
            [neo.vec.euler :as euler]
            [neo.vec.vec3 :as vec3]
            [neo.vec.vec4 :as vec4]
            [neo.vec.mat4 :as mat4]
            [neo.vec.quaternion :as quat]
            [neo.test-helpers :refer [is= roughly= are-elements-roughly= *eps* elements-roughly=]]))

(def x 2)
(def y 3)
(def z 4)
(def w 5)

(deftest setFromValues-test
  (let [a (vec3/vec3)]
    (is= 0 (vx a))
    (is= 0 (vy a))
    (is= 0 (vz a))
    (vec3/setFromValues a x y z)
    (is= x (vx a))
    (is= y (vy a))
    (is= z (vz a))))

(deftest copy-test
  (let [a (vec3/vec3 x y z)
        b (vec3/copy (vec3/vec3) a)]
    (is= (vx a) (vx b))
    (is= (vy a) (vy b))
    (is= (vz a) (vz b))
    (aset a 0 0)
    (aset a 1 -1)
    (aset a 2 -2)
    (is= x (vx b))
    (is= y (vy b))
    (is= z (vz b))))

(deftest arithmetic-test
  (testing "add"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 (- x) (- y) (- z))]
      (vec3/add a b)
      (is= 0 (vx a))
      (is= 0 (vy a))
      (is= 0 (vz a))
      (let [c (vec3/add b b (vec3/vec3))]
        (is= (* -2 x) (vx c))
        (is= (* -2 y) (vy c))
        (is= (* -2 z) (vz c)))))
  (testing "addScaledVector"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 x y z)
          s 3]
      (vec3/addScaledVector a b s)
      (is= (vx a) (+  x (* (vx b) s)))
      (is= (vy a) (+  y (* (vy b) s)))
      (is= (vz a) (+  z (* (vz b) s)))))
  (testing "sub"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 (- x) (- y) (- z))]
      (vec3/sub a b)
      (is= (* 2 x) (vx a))
      (is= (* 2 y) (vy a))
      (is= (* 2 z) (vz a))
      (let [c (vec3/sub a a (vec3/vec3))]
        (are-elements-roughly= c [0 0 0]))))
  (testing "'multiplyVectors'"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 2 3 -5)
          c (vec3/mult a b (vec3/vec3))]
      (is= (vx c) (* x 2))
      (is= (vy c) (* y 3))
      (is= (vz c) (* z -5))))
  (testing "mult,div"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 (* 2 x) (* 2 y) (* 2 z))
          c (vec3/vec3 (* 4 x) (* 4 y) (* 4 z))]
      (vec3/mult a b)
      (is= (vx a) (* x (vx b)))
      (is= (vy a) (* y (vy b)))
      (is= (vz a) (* z (vz b)))
      (vec3/div b c)
      (is (< (m/abs (- (vx b) 0.5)) *eps*))
      (is (< (m/abs (- (vy b) 0.5)) *eps*))
      (is (< (m/abs (- (vz b) 0.5)) *eps*)))))


(deftest applyQuaternion-test
  (let [a (vec3/vec3 x y z)
        _(vec3/applyQuaternion a (quat/quaternion))]
    (is= (vx a) x)
    (is= (vy a) y)
    (is= (vz a) z)
    (vec3/applyQuaternion a (quat/quaternion x y z w))
    (is= (vx a) 108)
    (is= (vy a) 162)
    (is= (vz a) 216)))

(deftest applyMat4-test
  (let [a (vec3/vec3 x y z)
        b (vec4/vec4 x y z 1)
        m (mat4/makeRotateX (mat4/create) pi)]
    (vec3/applyMat4 a m)
    (vec4/applyMat4 b m)
    (is (roughly= (vx a) (/ (vx b) (vw b))))
    (is (roughly= (vy a) (/ (vy b) (vw b))))
    (is (roughly= (vz a) (/ (vz b) (vw b))))
    (mat4/makeTranslation m 3 2 1)
    (vec3/applyMat4 a m)
    (vec4/applyMat4 b m)
    (is (roughly= (vx a) (/ (vx b) (vw b))))
    (is (roughly= (vy a) (/ (vy b) (vw b))))
    (is (roughly= (vz a) (/ (vz b) (vw b))))
    (mat4/setFromValues m 1 0 0 0  0 1 0 0  0 0 1 1  0 0 0 0)
    (vec3/applyMat4 a m)
    (vec4/applyMat4 b m)
    (is (roughly= (vx a) (/ (vx b) (vw b))))
    (is (roughly= (vy a) (/ (vy b) (vw b))))
    (is (roughly= (vz a) (/ (vz b) (vw b))))))

(deftest cross-test
  (testing "cross->cross arity"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 (* 2 x) (- y) (* 0.5 z))
          control (vec3/vec3 18 12 -18)]
      (vec3/cross a b)
      (are-elements-roughly= a control)))
  (testing "cross"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 x (- y) z)
          c (vec3/vec3)
          control (vec3/vec3 24 0 -12)]
      (vec3/cross a b c)
      (are-elements-roughly= c control))))

(deftest normalize-test
  (let [a (vec3/vec3 x 0 0)
        b (vec3/vec3 0 (- y) 0)
        c (vec3/vec3 0 0 z)]
    (vec3/normalize a)
    (is= 1 (vec3/length a))
    (is= 1 (vx a))
    (vec3/normalize b)
    (is= 1 (vec3/length b))
    (is= -1 (vy b))
    (vec3/normalize c)
    (is= 1 (vec3/length c))
    (is= 1 (vz c))))

(deftest length-test
  (testing "length"
    (let [a (vec3/vec3 x 0 0)
          b (vec3/vec3 0 (- y) 0)
          c (vec3/vec3 0 0 z)
          d (vec3/vec3)]
      (is= (vec3/length a) x)
      (is= (vec3/lengthSq a) (* x x))
      (is= (vec3/length b) y)
      (is= (vec3/lengthSq b) (* y y))
      (is= (vec3/length c) z)
      (is= (vec3/lengthSq c) (* z z))
      (is= (vec3/length d) 0)
      (is= (vec3/lengthSq d) 0)
      (vec3/setFromValues a x y z)
      (is= (vec3/length a) (m/sqrt (+ (* x x) (* y y) (* z z))))
      (is= (vec3/lengthSq a) (+ (* x x) (* y y) (* z z)))))
  (testing "setLength"
    (let [a (vec3/vec3 x 0 0)]
      (is= x (vec3/length a))
      (vec3/setLength a y)
      (is= y (vec3/length a))
      (vec3/setFromValues a 0 0 0)
      (is= 0 (vec3/length a))
      ; (vec3/setLength a y)
      ; (is= y (vec3/length a))
      ; (vec3/setLength a nil)
      ; (is (js/isNaN (vec3/length a)))
      )))

(deftest scalar-ops-test
  (testing "setScalar, addScalar,subScalar"
    (let [a (vec3/vec3)
          s 3]
      (vec3/setScalar a s)
      (are-elements-roughly= a [s s s])
      (vec3/addScalar a s)
      (are-elements-roughly= a [(* 2 s) (* 2 s) (* 2 s)])
      (vec3/subScalar a (* 2 s))
      (are-elements-roughly= a [0 0 0])))
  (testing "multScalar, divScalar"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 (- x) (- y) (- z))]
      (vec3/multScalar a (- 2))
      (are-elements-roughly= a [(* -2 x) (* -2 y) (* -2 z)])
      (vec3/multScalar b (- 2))
      (are-elements-roughly= b [(* 2 x) (* 2 y) (* 2 z)])
      (vec3/divScalar a (- 2))
      (are-elements-roughly= a [x y z])
      (vec3/divScalar b (- 2))
      (are-elements-roughly= b [(- x) (- y) (- z)]))))

(deftest setFromMatrix-test
  (testing "setFromMatrixPosition"
    (let [a (vec3/vec3)
          m (mat4/createFromValues 2 11 23 41
                                   3 13 29 43
                                   5 17 31 47
                                   7 19 37 53)]
      (vec3/setFromMatrixPosition a m)
      (are-elements-roughly= a [7 19 37])))
  (testing "setFromMatrixScale"
    (let [a (vec3/vec3)
          m (mat4/createFromValues 2 11 23 41
                                   3 13 29 43
                                   5 17 31 47
                                   7 19 37 53)
          control [ 25.573423705088842, 31.921779399024736, 35.70714214271425 ]]
      (vec3/setFromMatrixScale a m)
      (are-elements-roughly= a control)))
  (testing "setFromMatrixColumn"
    (let [a (vec3/vec3)
          m (mat4/createFromValues 2 11 23 41
                                   3 13 29 43
                                   5 17 31 47
                                   7 19 37 53)]
      (vec3/setFromMatrixColumn a m 0)
      (are-elements-roughly= a [2 11 23])
      (vec3/setFromMatrixColumn a m 2)
      (are-elements-roughly= a [5 17 31]))))

(deftest lerp-test
  (let [a (vec3/vec3 x 0 z)
        b (vec3/vec3 0 (- y) 0)]
    (are-elements-roughly= (vec3/lerp a a 0) (vec3/lerp a a 0.5))
    (are-elements-roughly= (vec3/lerp a a 0) (vec3/lerp a a 1))
    (are-elements-roughly= (vec3/lerp (vec3/vclone a) b 0) a)
    (is (roughly= (vx (vec3/lerp (vec3/vclone a) b 0.5)) (* x 0.5)))
    (is (roughly= (vy (vec3/lerp (vec3/vclone a) b 0.5)) (* (- y) 0.5)))
    (is (roughly= (vz (vec3/lerp (vec3/vclone a) b 0.5)) (* z 0.5)))
    (are-elements-roughly= (vec3/lerp (vec3/vclone a) b 1) b)))

(deftest misc-test
  (testing "dot"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 (- x) (- y) (- z))
          c (vec3/vec3)]
      (is= (vec3/dot a b) (+ (* (- x) x) (* (- y) y)  (* (- z) z)))
      (is= (vec3/dot a c) 0)))
  (testing "negate"
    (are-elements-roughly= [(- x) (- y) (- z)] (vec3/negate (vec3/vec3 x y z))))
  (testing "min/max/clamp"
    (let [a (vec3/vec3 x y z)
          b (vec3/vec3 (- x) (- y) (- z))
          c (vec3/vec3)]
      (vec3/vmin b a c)
      (are-elements-roughly= c [(- x) (- y) (- z)])
      (vec3/vmax b a c)
      (are-elements-roughly= c [x y z])
      (vec3/setFromValues c (* -2 x) (* 2 y) (* -2 z))
      (vec3/clamp c b a)
      (are-elements-roughly= c [(- x) y (- z)])))
  (testing "clampScalar"
    (let [a (vec3/vec3 -0.01 0.5 1.5)
          control (vec3/vec3 0.1 0.5 1.0)]
      (vec3/clampScalar a 0.1 1.0)
      (are-elements-roughly= a control)))
  (testing "distanceTo/distanceToSquared"
    (let [a (vec3/vec3 x 0 0)
          b (vec3/vec3 0 (- y) 0)
          c (vec3/vec3 0 0 z)
          d (vec3/vec3)]
      (is= x (vec3/distance a d))
      (is= (* x x) (vec3/distanceSq a d))
      (is= y (vec3/distance b d))
      (is= (* y y) (vec3/distanceSq b d))
      (is= z (vec3/distance c d))
      (is= (* z z) (vec3/distanceSq c d))))
  (testing "manhattanLength"
    (let [a (vec3/vec3 x 0 0)
          b (vec3/vec3 0 (- y) 0)
          c (vec3/vec3 0 0 z)
          d (vec3/vec3)]
      (is= x (vec3/manhattanLength a))
      (is= y (vec3/manhattanLength b))
      (is= z (vec3/manhattanLength c))
      (is= 0 (vec3/manhattanLength d))
      (vec3/setFromValues a x y z)
      (is= (+ (m/abs x) (m/abs y) (m/abs z)) (vec3/manhattanLength a))))
  (testing  "reflect"
    (let [a (vec3/vec3 0 -1 0)
          normal (vec3/vec3 0 1 0)
          b (vec3/vec3)]
      (are-elements-roughly= (vec3/reflect a normal b) [0 1 0])
      (vec3/setFromValues a 1 -1 0)
      (are-elements-roughly= (vec3/reflect a normal b) [1 1 0])
      (vec3/setFromValues a 1 -1 0)
      (vec3/setFromValues normal 0 -1 0)
      (are-elements-roughly= (vec3/reflect a normal b) [1 1 0])))
  (testing "angleTo"
    (let [a (vec3/vec3 0, -0.18851655680720186, 0.9820700116639124)
          b (vec3/vec3 0, 0.18851655680720186, -0.9820700116639124)]
      (is= 0 (vec3/angleTo a a))
      (is= pi (vec3/angleTo a b) (vec3/angleTo b a))
      (let [x (vec3/vec3 1 0 0)
            y (vec3/vec3 0 1 0)
            z (vec3/vec3 0 0 1)]
        (is (roughly= (vec3/angleTo x y) (/ pi 2)))
        (is (roughly= (vec3/angleTo x z) (/ pi 2)))
        (is (roughly= (vec3/angleTo z x) (/ pi 2)))
        (is (< (- (vec3/angleTo x (vec3/vec3 1 1 0)) (/ pi 4)) *eps*))))))

