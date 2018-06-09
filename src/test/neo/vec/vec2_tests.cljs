(ns neo.vec.vec2-tests
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

(deftest vector-arithmetic-test
  (testing "add"
    (let [a (vec2/vec2 x y)
          b (vec2/vec2 (- x) (- y))]
      (vec2/add a b)
      (are-elements-roughly= a [0 0])
      (let [c (vec2/add (vec2/vec2) b b)]
        (are-elements-roughly= c [(* x (- 2)) (* y (- 2))]))))
  (testing "sub"
    (let [a (vec2/vec2 x y)
          b (vec2/vec2 (- x) (- y))]
      (vec2/sub a b)
      (are-elements-roughly= a [(* x 2) (* y 2)])
      (let [c (vec2/sub (vec2/vec2) a a)]
        (are-elements-roughly= c [0 0]))))
  (testing "multiply/divide"
    (let [a (vec2/vec2 x y)
          b (vec2/vec2 (* 2 x) (* 2 y))
          c (vec2/vec2 (* 4 x) (* 4 y))]
      (vec2/mult a b)
      (are-elements-roughly= a [(* x (vx b)) (* y (vy b))])
      (vec2/div b c)
      (are-elements-roughly= b [0.5 0.5]))))

(deftest scalar-arithmetic-test
  (testing "set/add/subScalar"
    (let [a (vec2/vec2 1 1)
          s 3]
      (vec2/setScalar a s)
      (are-elements-roughly= a [s s])
      (vec2/addScalar a s)
      (are-elements-roughly= a [(* s 2)(* s 2)])
      (vec2/subScalar a (* 2 s))
      (are-elements-roughly= a [0 0])))
  (testing "mult/divScalar"
    (let [a (vec2/vec2 x y)
          b (vec2/vec2 (- x) (- y))]
      (vec2/multScalar a (- 2))
      (are-elements-roughly= a [(* x (- 2))(* y (- 2))])
      (vec2/multScalar b (- 2))
      (are-elements-roughly= b [(* x 2)(* y 2)])
      (vec2/divScalar a (- 2))
      (are-elements-roughly= a [x y])
      (vec2/divScalar b (- 2))
      (are-elements-roughly= b [(- x) (- y)])))
  (testing "addScaledVector"
    (let [a (vec2/vec2 x y)
          b (vec2/vec2 2 3)
          s 3]
      (vec2/addScaledVector a b s)
      (are-elements-roughly= a [(+ x (* s (vx b))) (+ y (* s (vy b)))]))))

(deftest misc-test
  (testing "negate"
    (let [a (vec2/vec2 x y)]
      (vec2/negate a)
      (are-elements-roughly= a [(- x) (- y)])))
  (testing "dot"
    (let [a (vec2/vec2 x y)
          b (vec2/vec2 (- x) (- y))
          c (vec2/vec2)]
      (is= (vec2/dot a b) (+ (* (- x) x) (* (- y) y)))
      (is= (vec2/dot a c) 0)))
  (testing "manhattanLength"
    (let [a (vec2/vec2 x 0)
          b (vec2/vec2 0 (- y))
          c (vec2/vec2)]
      (is= (vec2/manhattanLength a) x)
      (is= (vec2/manhattanLength b) y)
      (is= (vec2/manhattanLength c) 0)
      (vec2/setFromValues a x y)
      (is= (vec2/manhattanLength a) (+ (m/abs x) (m/abs y)))))
  (testing "normalize"
    (let [a (vec2/vec2 x 0)
          b (vec2/vec2 0 (- y))
          c (vec2/vec2)]
      (is (not= (vec2/length a) 1))
      (vec2/normalize a)
      (is= (vec2/length a) 1)
      (is= (vx a) 1)
      (is (not= (vec2/length b) 1))
      (vec2/normalize b)
      (is= (vec2/length b) 1)
      (is= (vy b) (- 1))))
  (testing "setLength"
    (let [a (vec2/vec2 x 0)]
      (is= (vec2/length a) x)
      (vec2/setLength a y)
      (is= (vec2/length a) y)))
  (testing "length, lengthSq"
    (let [a (vec2/vec2 x 0)
          b (vec2/vec2 0 (- y))
          c (vec2/vec2)]
      (is= x (vec2/length a))
      (is= (* x x) (vec2/lengthSq a))
      (is= y (vec2/length b))
      (is= (* y y) (vec2/lengthSq b))
      (is= 0 (vec2/length c))
      (is= 0 (vec2/lengthSq c))
      (vec2/setFromValues a x y)
      (is= (m/sqrt (+ (* x x) (* y y))) (vec2/length a))
      (is= (+ (* x x) (* y y)) (vec2/lengthSq a))))
  (testing "distance, distanceSq"
    (let [a (vec2/vec2 x 0)
          b (vec2/vec2 0 (- y))
          c (vec2/vec2)]
      (is= x (vec2/distance a c))
      (is= (* x x) (vec2/distanceSq a c))
      (is= y (vec2/distance b c))
      (is= (* y y) (vec2/distanceSq b c)))))

(deftest min-max-clamp-test
  (let [a (vec2/vec2 x y)
        b (vec2/vec2 (- x) (- y))
        c (vec2/vec2)]
    (vec2/vmin (vec2/copy c a) b)
    (are-elements-roughly= c [(- x) (- y)])
    (vec2/vmax (vec2/copy c a) b)
    (are-elements-roughly= c [x y])
    (vec2/setFromValues c (* (- 2) x) (* 2 y))
    (vec2/clamp c b a)
    (are-elements-roughly= c [(- x) y])
    (vec2/setFromValues c (* (- 2) x) (* 2 x))
    (vec2/clampScalar c (- x) x)
    (are-elements-roughly= c [(- x) x])))

(deftest rounding
  (testing "floor"
    (are-elements-roughly= (vec2/floor (vec2/vec2 -0.1 0.1)) [-1 0])
    (are-elements-roughly= (vec2/floor (vec2/vec2 -0.5 0.5)) [-1 0])
    (are-elements-roughly= (vec2/floor (vec2/vec2 -0.9 0.9)) [-1 0]))
  (testing "ceil"
    (are-elements-roughly= (vec2/ceil (vec2/vec2 -0.1 0.1)) [0 1])
    (are-elements-roughly= (vec2/ceil (vec2/vec2 -0.5 0.5)) [0 1])
    (are-elements-roughly= (vec2/ceil (vec2/vec2 -0.9 0.9)) [0 1]))
  ;;missing roundToZero
  (testing "round"
    (are-elements-roughly= (vec2/round (vec2/vec2 -0.1 0.1)) [0 0])
    (are-elements-roughly= (vec2/round (vec2/vec2 -0.5 0.5)) [0 1])
    (are-elements-roughly= (vec2/round (vec2/vec2 -0.9 0.9)) [-1 1])))

(deftest lerp-test
  (let [a (vec2/vec2 x 0)
        b (vec2/vec2 0 (- y))]
    (are-elements-roughly= (vec2/lerp a a 0) (vec2/lerp a a 0.5))
    (are-elements-roughly= (vec2/lerp a a 0) (vec2/lerp a a 1))
    (are-elements-roughly= (vec2/lerp (vec2/vclone a) b 0) a)
    (are-elements-roughly= (vec2/lerp (vec2/vclone a) b 0.5) [(* 0.5 x)(* 0.5 (- y))])
    (are-elements-roughly= (vec2/lerp (vec2/vclone a) b 1) b)))
