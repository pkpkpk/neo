(ns neo.vec.mat4-tests
  (:require-macros [neo.macros :refer [set- vx vy vz vsetz+]])
  (:require [cljs.test :refer-macros [deftest is testing async are]]
            [neo.math :as m :include-macros true :refer [pi -pi]]
            [neo.vec.euler :as euler]
            [neo.vec.vec3 :as vec3]
            [neo.vec.mat4 :as mat4]
            [neo.vec.quaternion :as quat]
            [neo.test-helpers :refer [is= roughly= are-elements-roughly= *eps*
                                      is-euler-roughly= elements-roughly=]]))

(defn log [& args] (.apply js/console.log js/console (into-array args)))

(deftest testsetFromValues []
  (let [mat (mat4/ident)]
    (is (= 1 (mat4/determinant mat)))
    (mat4/setFromValues mat 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    (doseq [n (range 16)]
      (is (= n (aget mat n))))))

(deftest ident-test
  (let [mat (mat4/ident)
        control (mat4/ident)]
    (is (= 1 (mat4/determinant mat)))
    (mat4/setFromValues mat 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    (are-elements-roughly= control (mat4/ident mat))))

(deftest clone-test
  (let [a (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
        b (mat4/mclone a)]
    (are-elements-roughly= a b)
    (aset a 2 99)
    (is (not (elements-roughly= a b)))))

(deftest transpose-test
  (let [a (mat4/ident)
        b (mat4/transpose (mat4/mclone a ))]
    (are-elements-roughly= a b)
    (let [c (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          d (mat4/transpose (mat4/mclone c))]
      (is (not (elements-roughly= c d)))
      (mat4/transpose d)
      (are-elements-roughly= c d))))

(deftest copy-test
  (let [a (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
        b (mat4/ident)
        _ (mat4/copy b a)]
    (are-elements-roughly= a b)
    (aset a 2 99)
    (is (not (elements-roughly= a b)))))

(deftest copyPosition-test
  (let [a (mat4/createFromValues 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
        b (mat4/createFromValues 1 2 3 4 5 6 7 8 9 10 11 12 -5 -4 -3 16)]
    (is (not (elements-roughly= a b)))
    (mat4/copyPosition b a)
    (are-elements-roughly= a b)))

(deftest makeRotationAxis-test
  (let [axis (vec3/normalize (vec3/vec3 1.5 0 1.0))
        radians (m/deg->rad 45)
        a (mat4/rotateAxis (mat4/create) axis radians)
        control (mat4/createFromValues
                 0.9098790095958609
                 0.39223227027636803
                 0.13518148560620882
                 0

                 -0.39223227027636803
                 0.7071067811865476
                 0.588348405414552
                 0

                 0.13518148560620882
                 -0.588348405414552
                 0.7972277715906868
                 0

                 0 0 0 1)]
    (are-elements-roughly= a control)))

(deftest lookAt-test
  (let [a (mat4/ident)
        control (mat4/ident)
        eye (vec3/vec3 0 0 0)
        target (vec3/vec3 0 1 -1)
        up (vec3/vec3 0 1 0)
        _(mat4/lookAt a eye target up)
        rotation (euler/setFromRotationMatrix (euler/euler) a)]
    (is (= (* (.-x rotation) (/ 180 pi)) 45))
    (testing "eye and target are in the same position"
      (vec3/copy eye target)
      (mat4/lookAt a eye target up)
      (are-elements-roughly= a control))
    (testing "up + z parallel"
      (mat4/setFromValues control
                          1 0 0 0
                          0 0.0001 -1 0
                          0 1 0.0001 0
                          0 0 0 1)
      (vec3/setFromValues eye 0 1 0)
      (vec3/setFromValues target 0 0 0)
      (mat4/lookAt a eye target up)
      (are-elements-roughly= a control))))

(deftest det-test
  (let [m (mat4/createFromValues 2 -1 6 -8
                                 3 -21 7 -9
                                 4 -3 8 -10
                                 5 -4 10 -12)]
    (is= 76 (mat4/determinant m))
    (is= 1 (mat4/determinant (mat4/ident)))))

(deftest makeRotationFromEuler-test
  (let [es [(euler/euler 0 0 0 "XYZ")
            (euler/euler 1 0 0 "XYZ")
            (euler/euler -pi 0 -pi "XYZ")]]
    (doseq [e es]
      (let [m (mat4/makeRotationFromEuler (mat4/mat4) e)
            e2 (euler/setFromRotationMatrix (euler/euler) m (.-order e))
            m2 (mat4/makeRotationFromEuler (mat4/mat4) e2)]
        (are-elements-roughly= m m2)
        (is-euler-roughly= e e2)
        (let [m3 (mat4/extractRotation m2)
              e3 (euler/setFromRotationMatrix (euler/euler) m3 (.-order e))]
          (are-elements-roughly= m m3)
          (is-euler-roughly= e e3))))))

(deftest makeOrthographic-test
  (let [a (mat4/makeOrthographic (mat4/create) -1 1 -1 1 1 100)
        control (mat4/createFromValues 1 0 0 0
                                       0 -1 0 0
                                       0 0 (/ -2 99) 0
                                       0 0 (/ -101 99) 1)]
    (are-elements-roughly= a control)))

(deftest invert-test
  (let [mats [(mat4/rotateX (mat4/create) 0.3)
              (mat4/rotateX (mat4/create) -0.3)
              (mat4/rotateY (mat4/create) 0.3)
              (mat4/rotateY (mat4/create) -0.3)
              (mat4/rotateZ (mat4/create) 0.3)
              (mat4/rotateZ (mat4/create) -0.3)
              (mat4/scale (mat4/create) 1 2 3)
              (mat4/scale (mat4/create) (/ 1 8) (/ 1 2) (/ 1 3))
              (mat4/makePerspective (mat4/create) -1 1 1 -1 1 1000)
              (mat4/makePerspective (mat4/create) -16 16 9 -9 0.1 10000)
              (mat4/translate (mat4/create) 1 2 3)]]
    (doseq [m mats]
      (let [mInv (mat4/create)
            _(mat4/invert  m mInv)
            mSelfInverse (mat4/mclone m)]
        (mat4/invert mSelfInverse)
        (are-elements-roughly= mInv mSelfInverse)
        (testing "det should be reciprocal"
          (is (< (m/abs (- (* (mat4/determinant m) (mat4/determinant mInv)) 1)) *eps*)))
        (testing "multiplying inverses should return identity"
          (let [product (mat4/mult m mInv (mat4/create))]
            (is (< (- (mat4/determinant product) 1) *eps*))
            (are-elements-roughly= product (mat4/ident))))))))

(deftest compose+decompose-test
  (let [translations [(vec3/vec3)
                      (vec3/vec3 3 0 0 )
                      (vec3/vec3 0 4 0 )
                      (vec3/vec3 0 0 5 )
                      (vec3/vec3 -6 0 0 )
                      (vec3/vec3 0 -7 0 )
                      (vec3/vec3 0 0 -8 )
                      (vec3/vec3 -2 5 -9 )
                      (vec3/vec3 -2 -5 -9 )]
        scales [(vec3/vec3 1 1 1)
                (vec3/vec3 2 2 2)
                (vec3/vec3 1 -1 1)
                (vec3/vec3 -1 1 1)
                (vec3/vec3 1 1 -1)
                (vec3/vec3 2 -2 1)
                (vec3/vec3 -1 2 -2)
                (vec3/vec3 -1 -1 -1)
                (vec3/vec3 -2 -2 -2)]
        rotations [(quat/quaternion)
                   (quat/setFromEuler (quat/quaternion) (euler/euler 1 1 0))
                   (quat/setFromEuler (quat/quaternion) (euler/euler 1 -1 0))
                   (quat/quaternion 0, 0.9238795292366128, 0, 0.38268342717215614)]]
    (doseq [t translations]
      (doseq [s scales]
        (doseq [r rotations]
          (let [m (mat4/compose (mat4/create) t r s)
                t2 (vec3/vec3)
                r2 (quat/quaternion)
                s2 (vec3/vec3)
                _ (mat4/decompose m t2 r2 s2)
                m2 (mat4/compose (mat4/create) t2 r2 s2)]
            (are-elements-roughly= m m2)))))))

(deftest shortcuts-test
  (testing "translate!"
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          [x y z] [10 20 30]
          CONTROL (mat4/mult m (mat4/translate x y z)  (mat4/create))
          TEST (mat4/translate! m x y z)]
      (are-elements-roughly= TEST CONTROL)))
  (testing "pre-translate!"
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          [x y z] [10 20 30]
          CONTROL (mat4/mult (mat4/translate x y z) m  (mat4/create))
          TEST (mat4/pre-translate! m x y z)]
      (are-elements-roughly= TEST CONTROL)))

  (testing "scale!"
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          [x y z] [10 20 30]
          CONTROL (mat4/mult m (mat4/scale x y z)  (mat4/create))
          TEST (mat4/scale! m x y z)]
      (are-elements-roughly= TEST CONTROL)))

  (testing "pre-scale!"
    (testing "arity-1"
      (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
            x 99.9
            CONTROL (mat4/mult (mat4/scale x x 1) m  (mat4/create))
            TEST (mat4/pre-scale! m x)]
        (are-elements-roughly= TEST CONTROL)))
    (testing "arity-2"
      (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
            x 99.9
            y 102.7
            CONTROL (mat4/mult (mat4/scale x y 1) m  (mat4/create))
            TEST (mat4/pre-scale! m x y)]
        (are-elements-roughly= TEST CONTROL)))
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          [x y z] [10 20 30]
          CONTROL (mat4/mult (mat4/scale x y z) m  (mat4/create))
          TEST (mat4/pre-scale! m x y z)]
      (are-elements-roughly= TEST CONTROL)))

  (testing "rotateX!"
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          angle (/ -pi 4)
          CONTROL (mat4/mult m (mat4/rotateX angle) (mat4/create))
          TEST (mat4/rotateX! m angle)]
      (are-elements-roughly= TEST CONTROL)))
  (testing "rotateY!"
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          angle (/ -pi 4)
          CONTROL (mat4/mult m (mat4/rotateY angle) (mat4/create))
          TEST (mat4/rotateY! m angle)]
      (are-elements-roughly= TEST CONTROL)))
  (testing "rotateZ!"
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          angle (/ -pi 4)
          CONTROL (mat4/mult m (mat4/rotateZ angle) (mat4/create))
          TEST (mat4/rotateZ! m angle)]
      (are-elements-roughly= TEST CONTROL)))
  (testing "shear!"
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          [x y z] [10 20 30]
          CONTROL (mat4/mult m (mat4/shear x y z)  (mat4/create))
          TEST (mat4/shear! m x y z)]
      (are-elements-roughly= TEST CONTROL)))
  (testing "pre-shear!"
    (let [m (mat4/createFromValues 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          [x y z] [10 20 30]
          CONTROL (mat4/mult (mat4/shear x y z) m (mat4/create))
          TEST (mat4/pre-shear! m x y z)]
      (are-elements-roughly= TEST CONTROL))))