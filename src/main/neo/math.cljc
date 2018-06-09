(ns neo.math "just some sugar.")

#?(:cljs (def pi ^:const js/Math.PI))
#?(:cljs (def -pi ^:const (js* "-Math.PI")))
#?(:cljs (def e js/Math.E))

#?(:cljs (def rad2deg (/ 180 pi)))
#?(:cljs (def deg2rad (/ 1 (/ 180 pi))))

#?(:cljs (defn rad->deg [radians] (* radians rad2deg)))
#?(:cljs (defn deg->rad [degrees] (* degrees deg2rad)))

(defmacro sin [theta]
  `(~'js/Math.sin ~theta))

(defmacro cos [theta]
  `(js/Math.cos ~theta))

(defmacro tan [theta]
  `(~'js/Math.tan ~theta))

(defmacro asin [theta]
  `(~'js/Math.asin ~theta))

(defmacro acos [theta]
  `(~'js/Math.acos ~theta))

(defmacro atan [theta]
  `(~'js/Math.atan ~theta))

(defmacro atanh [theta]
  `(~'js/Math.atanh ~theta))

(defmacro atan2 [y x]
  `(~'js/Math.atan2 ~y ~x))

(defmacro pow [base exp]
  `(~'js/Math.pow ~base ~exp))

(defmacro exp [x]
  `(~'js/Math.exp ~x))

(defmacro ln [x]
  `(~'js/Math.log ~x))

(defmacro log [x]
  `(~'js/Math.log10 ~x))

(defmacro sqrt [x]
  `(~'js/Math.sqrt ~x))

(defmacro cbrt [x]
  `(~'js/Math.cbrt ~x))

(defmacro floor [x]
  `(~'js/Math.floor ~x))

(defmacro ceil [x]
  `(~'js/Math.ceil ~x))

(defmacro abs [x]
  `(~'js/Math.abs ~x))

(defmacro round [x]
  `(~'js/Math.round ~x))

