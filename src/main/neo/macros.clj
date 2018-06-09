(ns neo.macros
  (:require [cljs.compiler :as comp]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [clojure.string :as string]))

(defmacro set+
  "+=, (set+ (.-foo o) 42) ;=> (set! (.-foo o) (+ (.-foo o) 42))"
  [access-form value]
  `(set! ~access-form  (+ ~access-form ~value)))

(defmacro set-
  "-=, (set- (.-foo o) 42) ;=> (set! (.-foo o) (- (.-foo o) 42))"
  [access-form value]
  `(set! ~access-form  (- ~access-form ~value)))

(defmacro set*
  "*=, (set+ (.-foo o) 42) ;=> (set! (.-foo o) (* (.-foo o) 42))"
  [access-form value]
  `(set! ~access-form  (* ~access-form ~value)))

(defmacro aset* [array-symbol index value]
  `(aset ~array-symbol ~index (* (aget ~array-symbol ~index) ~value)))

(defmacro aset+ [array-symbol index value]
  `(aset ~array-symbol ~index (+ (aget ~array-symbol ~index) ~value)))

(defmacro aset- [array-symbol index value]
  `(aset ~array-symbol ~index (- (aget ~array-symbol ~index) ~value)))

(defmacro vx [v] `(aget ~v 0))
(defmacro vy [v] `(aget ~v 1))
(defmacro vz [v] `(aget ~v 2))
(defmacro vw [v] `(aget ~v 3))

(defmacro vsetx [v x] `(aset ~v 0 ~x))
(defmacro vsety [v y] `(aset ~v 1 ~y))
(defmacro vsetz [v z] `(aset ~v 2 ~z))
(defmacro vsetw [q w] `(aset ~q 3 ~w))

(defmacro vsetx+ [v x] `(aset ~v 0 (+ (aget ~v 0)  ~x)))
(defmacro vsety+ [v y] `(aset ~v 1 (+ (aget ~v 1)  ~y)))
(defmacro vsetz+ [v z] `(aset ~v 2 (+ (aget ~v 2)  ~z)))
(defmacro vsetw+ [v w] `(aset ~v 3 (+ (aget ~v 3)  ~w)))

(defmacro vsetx- [v x] `(aset ~v 0 (- (aget ~v 0)  ~x)))
(defmacro vsety- [v y] `(aset ~v 1 (- (aget ~v 1)  ~y)))
(defmacro vsetz- [v z] `(aset ~v 2 (- (aget ~v 2)  ~z)))
(defmacro vsetw- [v w] `(aset ~v 3 (- (aget ~v 3)  ~w)))

(defmacro vsetx* [v x] `(aset ~v 0 (* (aget ~v 0)  ~x)))
(defmacro vsety* [v y] `(aset ~v 1 (* (aget ~v 1)  ~y)))
(defmacro vsetz* [v z] `(aset ~v 2 (* (aget ~v 2)  ~z)))
(defmacro vsetw* [v w] `(aset ~v 3 (* (aget ~v 3)  ~w)))

(defmacro goog-typedef
  "Define a custom type for use in JSDoc type annotations.

   docstring can be a simple type expression:
     `(good-typedef my-vector \"{!IVector}\")`

   Or you can use other tags by manually specifying a typedef tag:
     `(good-typedef my-string-array \"@typedef {!Array<string>}
                                      @implements {SomeProtocol}\")`

   Each annotation must occur on its own line with a space separating the tag
   and its type-expression"
  [sym docstring]
  ; (assert-args goog-typedef (core/string? docstring))
  (when (#{:error :warning} (get-in @env/*compiler* [:options :closure-warnings :check-types]))
    (let [typename (comp/munge (str *ns* "/" sym))
          docstring (if (string/starts-with? docstring "@typedef")
                      docstring
                      (str "@typedef{" docstring "}"))]
      `(do
         (declare ~(vary-meta sym assoc :tag 'symbol :typedef true))
         (~'js* ~(str "/** " docstring " */"))
         (~'js* ~(str typename))))))