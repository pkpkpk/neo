(ns neo.vec.affine-tests
  (:require [cljs.test :refer-macros [deftest is testing async are]]
            [neo.vec.affine :as affine]
            [neo.math :refer [pi -pi]]
            [goog.vec.Mat4 :as mat4]
            [neo.test-helpers :refer [is= roughly= are-elements-roughly=]]))

(defn ->v [m]
  (if (= 12 (alength m))
    [(aget m 0) (aget m 1) (aget m 2) 0
     (aget m 3) (aget m 4) (aget m 5) 0
     (aget m 6) (aget m 7) (aget m 8) 0
     (aget m 9) (aget m 10) (aget m 11) 1]
    (into [] (array-seq m))))


(def t #(affine/createFromValues 1 2 3  4 5 6  7 8 9  10 11 12))

(defn mat4t []
  (mat4/createFloat64FromValues 1 2 3 0  4 5 6 0  7 8 9 0  10 11 12 1))

(deftest basics-test
  (is (= [1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1]
         (->v (affine/ident))
         (->v (mat4/createFloat64Identity))))

  (testing "mult"
    (is= [30 36 42 0 66 81 96 0 102 126 150 0 148 182 216 1]
         (->v (affine/mult (t) (t)))
         (->v (mat4/multMat (mat4t) (mat4t) (mat4t)))))

  (is= "matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)" (affine/->CSS (affine/ident)))
  (is= "matrix3d(1,2,3,0,4,5,6,0,7,8,9,0,10,11,12,1)" (affine/->CSS (t)))
  (is=  (-> (affine/rotateX 180)
            (affine/mult (affine/translate 1 2 3))
            (affine/mult (affine/scale 4 5 6))
            (affine/mult (affine/shear 7 8 9))
            ->v)
        (-> (affine/rotateX 180)
            (affine/translate! 1 2 3)
            (affine/scale! 4 5 6)
            (affine/shear! 7 8 9)
            ->v)))

(deftest determinant-test
  (testing "affine determinant"
    (is= 1 (affine/det (affine/ident)))
    (is= 0 (affine/det (affine/createFromValues 1 1 1 1 1 1 1 1 1 1 1 1)))
    (is (roughly= -31 (affine/det (affine/scale -pi -pi -pi)) 0.01))))

(deftest invert-test
  (testing "affine invert"
    (is= (->v (affine/ident)) (->v (affine/invert (affine/ident))))
    (is (nil? (affine/invert (t))))
    (is= (->v (affine/invert (affine/scale .01 .01 .01)))
         (->v (affine/scale 100 100 100))))
  (testing "invertible?"
    (is (affine/invertible?  (affine/ident)))
    (is (affine/invertible?
         (affine/createFromValues 2 3 4, 5 6 7, 8 9 10, 11 12 13)))
    (is (not (affine/invertible?
              (affine/createFromValues js/NaN 3 4, 5 6 7, 8 9 10, 11 12 13))))
    (is (not (affine/invertible?
              (affine/createFromValues js/Infinity 3 4, 5 6 7, 8 9 10, 11 12 13))))))

(deftest **-test
  (let []
    (is= (->v (affine/** nil)) (->v (affine/ident)))
    (is= (->v (affine/** nil nil)) (->v (affine/ident)))
    (is= (->v (affine/** nil nil nil)) (->v (affine/ident)))
    (is= (->v (affine/** nil nil nil (affine/translate 5 5))) (->v (affine/translate 5 5)))
    (is= (-> (affine/translate 50 50)
             (affine/mult (affine/shear-x 45))
             (affine/mult (affine/shear-y 45))
             (affine/mult (affine/scale 1 2 3))
             ->v)
         (->v
          (affine/**
           (affine/translate 50 50)
           nil
           (affine/shear-x 45)
           (affine/shear-y 45)
           nil
           (affine/scale 1 2 3))))))

(deftest shear-test
  (is= (->v (affine/shear pi pi pi))
       (->v (affine/shear! (affine/ident) pi pi pi)))
  (is= (->v (affine/mult (t) (affine/shear pi pi pi)))
       (->v (affine/shear! (t) pi pi pi)))
  (is= (->v (affine/mult (affine/shear pi pi pi) (t)))
       (->v (affine/pre-shear! (t) pi pi pi))))

(deftest scale-test
  (testing "affine scale matrix"
    (is= [7 0 0 0 0 8 0 0 0 0 9 0 0 0 0 1]
         (->v (affine/scale 7 8 9))
         (->v (mat4/makeScale (mat4/createFloat64Identity) 7 8 9))))
  (testing "scale!"
    (is= [7 14 21 0 32 40 48 0 63 72 81 0 10 11 12 1]
         (->v (affine/scale! (t) 7 8 9))))
  (is (not= (->v (affine/mult (affine/scale 7 8 9) (t)))
            (->v (affine/mult (t) (affine/scale 7 8 9) ))))
  (testing "pre-scale! = (mult scale m)"
    (is (=  [7 16 27 0 28 40 54 0 49 64 81 0 70 88 108 1]
            (->v (affine/pre-scale! (t) 7 8 9))
            (->v (affine/mult (affine/scale 7 8 9) (t)))))))

(deftest translate-test
  (let []
    (testing "affine translation matrix"
      (is= [1 0 0 0 0 1 0 0 0 0 1 0 7 8 9 1]
           (->v (affine/translate 7 8 9))
           (->v (mat4/makeTranslate (mat4/createFloat64Identity) 7 8 9))))

    (testing "translate!"
      (is (= [1 2 3 0 4 5 6 0 7 8 9 0 112 137 162 1]
             (->v (affine/translate! (t) 7 8 9))
             (->v (affine/mult (t) (affine/translate 7 8 9))))))

    (is (not= (->v (affine/mult (affine/translate 7 8 9) (t)))
              (->v (affine/mult (t) (affine/translate 7 8 9)))))

    (testing "pre-translate! = (mult translate m)"
      (is (= [1 2 3 0 4 5 6 0 7 8 9 0 17 19 21 1]
             (->v (affine/pre-translate! (t) 7 8 9))
             (->v (affine/mult (affine/translate 7 8 9) (t))))))))

(deftest rotate-test
  (testing "rotate X, Y, Z"
    (let [theta (/ -pi 4)]
      (testing "rotateX"
        (are-elements-roughly=
         [1 0 0 0 0 0.70710678 -0.70710678 0 0 0.70710678 0.70710678 0 0 0 0 1]
         (->v (affine/rotateX theta))
         (->v (mat4/makeRotateX (mat4/createFloat64Identity) theta))))
      (testing "rotateY"
        (are-elements-roughly=
         [0.70710678 0 0.70710678 0 0 1 0 0 -0.70710678 0 0.70710678 0 0 0 0 1]
         (->v (affine/rotateY theta))
         (->v (mat4/makeRotateY (mat4/createFloat64Identity) theta))))
      (testing "rotateZ"
        (are-elements-roughly=
         [0.70710678 -0.70710678 0 0 0.70710678 0.70710678 0 0 0 0 1 0 0 0 0 1]
         (->v (affine/rotateZ theta))
         (->v (mat4/makeRotateZ (mat4/createFloat64Identity) theta))))))

  (testing "rotateX!"
    (let [theta (/ pi 4)
          CONTROL (-> (mat4/createFloat64Identity)
                      (goog.vec.Mat4.rotateX theta)
                      ->v)
          TEST (->v (affine/rotateX! (affine/ident) theta))
          MULT-PARITY (->v (affine/mult (affine/ident) (affine/rotateX theta)))]
      (are-elements-roughly=  CONTROL TEST MULT-PARITY)))

  (testing "rotateY!"
    (let [theta (/ pi 4)
          CONTROL (-> (mat4/createFloat64Identity)
                      (goog.vec.Mat4.rotateY theta)
                      ->v)
          TEST (->v (affine/rotateY! (affine/ident) theta))
          MULT-PARITY (->v (affine/mult (affine/ident) (affine/rotateY theta)))]
      (are-elements-roughly= CONTROL TEST MULT-PARITY)))

  (testing "rotateZ!"
    (let [theta (/ pi 4)
          CONTROL (-> (mat4/createFloat64Identity)
                      (goog.vec.Mat4.rotateZ theta)
                      ->v)
          TEST (->v (affine/rotateZ! (affine/ident) theta))
          MULT-PARITY (->v (affine/mult (affine/ident) (affine/rotateZ theta)))]
      (are-elements-roughly= CONTROL TEST MULT-PARITY)))

  (testing "rotate-axis (make-rotate)"
    (let [theta (/ pi 4)
          CONTROL (-> (goog.vec.Mat4.createFloat64)
                      (goog.vec.Mat4.makeRotate theta, 0, 0, 1)
                      ->v)
          TEST (->v (affine/rotate-axis  theta [0, 0, 1]))]
      (are-elements-roughly=
       [0.7071068, 0.7071068, 0, 0,
        -0.7071068, 0.7071068, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1]
       CONTROL
       TEST)))

  (testing "rotate-axis! (rotate)"
    (let [theta (/ pi 4)
          CONTROL (-> (mat4/createFloat64Identity)
                      (goog.vec.Mat4.rotate theta, 0, 0, 1)
                      ->v)
          TEST (->v (affine/rotate-axis! (affine/ident) theta [0, 0, 1]))
          MULT-PARITY (->v (affine/mult (affine/ident) (affine/rotate-axis  theta [0, 0, 1])))]
      (are-elements-roughly=
       [0.7071068, 0.7071068, 0, 0,
        -0.7071068, 0.7071068, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1]
       CONTROL
       TEST
       MULT-PARITY)
      (are-elements-roughly=
       (->v (goog.vec.Mat4.rotate (mat4t), theta, 0, 0, 1))
       (->v (affine/rotate-axis! (t) theta [0 0 1]))
       (->v (affine/mult (t) (affine/rotate-axis  theta [0, 0, 1]) )))))

  (are-elements-roughly=
   (->v (affine/rotateZ! (affine/ident) (/ js/Math.PI 4)))
   (->v (affine/rotate-axis! (affine/ident) (/ js/Math.PI 4) [0 0 1]))))
