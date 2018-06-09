(ns neo.styles
  #?(:cljs (:require-macros [neo.styles :as styles])))

(def styles {
    :display "block"
    :position "absolute"
    :width "100%"
    :height "100%"
    :boxSizing "border-box"
    :WebkitTransformOrigin "center center"
       :MozTransformOrigin "center center"
          :transformOrigin "center center"
    :WebkitBackfaceVisibility "hidden"
       :MozBackfaceVisibility "hidden"
          :backfaceVisibility "hidden"
    :WebkitTransformStyle "preserve-3d"
       :MozTransformStyle "preserve-3d"
          :TransformStyle "preserve-3d"})

(defn xfstyles [xf]
  {:WebkitTransform xf
   :transform xf
   :display "block"
   :position "absolute"
   :width "100%"
   :height "100%"
   :boxSizing "border-box"
   :WebkitTransformOrigin "center center"
      :MozTransformOrigin "center center"
         :transformOrigin "center center"
   :WebkitBackfaceVisibility "hidden"
      :MozBackfaceVisibility "hidden"
         :backfaceVisibility "hidden"
   :WebkitTransformStyle "preserve-3d"
      :MozTransformStyle "preserve-3d"
         :TransformStyle "preserve-3d"})

(def class-body
  "display:block;
   position:absolute;
   width:100%;
   height:100%;
   box-sizing: border-box;
   -webkit-transform-origin: center center;
      -moz-transform-origin: center center;
           transform-origin: center center;
   -webkit-backface-visibility: hidden;
      -moz-backface-visibility: hidden;
           backface-visibility: hidden;
   -webkit-transform-style: preserve-3d;
      -moz-transform-style: preserve-3d;
           transform-style: preserve-3d;")

(defn css-class
  ([](css-class "neo"))
  ([classname]
   (str "." classname "{" class-body "}")))
