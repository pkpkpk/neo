(ns neo.controls.trackball
  "A very literal translation of:
   https://github.com/mrdoob/three.js/blob/dev/examples/js/controls/TrackballControls.js"
  (:require-macros [neo.macros :refer [vx vy vz vsetx vsety]])
  (:require [neo.core :as neo]
            [neo.math :as m :include-macros true]
            [neo.vec.vec2 :as vec2 :refer [vector2]]
            [neo.vec.vec3 :as vec3]
            [neo.vec.quaternion :as quat]))

(defn log [& args] (.apply js/console.log js/console (into-array args)))

(def STATE #js {"NONE" -1,
                "ROTATE" 0,
                "ZOOM" 1,
                "PAN" 2,
                "TOUCH_ROTATE" 3
                "TOUCH_ZOOM_PAN" 4 })

(defn handleResize [this]
  (let [box (.getBoundingClientRect (.-domElement this))
        d (.. this -domElement -ownerDocument -documentElement)]
    (set! (.. this -screen -left) (+ (.-left box) (.-pageXOffset js/window) (- (.-clientLeft d))))
    (set! (.. this -screen -top) (+ (.-top box) (.-pageYOffset js/window) (- (.-clientTop d))))
    (set! (.. this -screen -width) (.-width box))
    (set! (.. this -screen -height) (.-height box))))


(def getMouseOnScreen
  (let [v (vector2)]
    (fn getMouseOnScreen [this pageX pageY]
      (let [x (/ (- pageX (.. this -screen -left)) (.. this  -screen -width))
            y (/ (- pageY (.. this -screen -top)) (.. this  -screen -height))]
        (vec2/setFromValues v x y)
        v))))

(def getMouseOnCircle
  (let [v (vector2)]
    (fn getMouseOnCircle [this pageX pageY]
      (let [w (.. this -screen -width)
            halfWidth (*  0.5 w)
            x (/ (- pageX  halfWidth (.. this -screen -left)) halfWidth)
            y (/ (+ (.. this -screen -height) (* 2 (- (.. this -screen -top) pageY))) w)]
        (vec2/setFromValues v x y)
        v))))

(defn rotateCamera
  "side-effects only"
  [this]
  (let [axis (vec3/vec3)
        quaternion (quat/quaternion)
        eyeDirection (vec3/vec3)
        objectUpDirection (vec3/vec3)
        objectSidewaysDirection (vec3/vec3)
        moveDirection (vec3/vec3)]
    (fn rotateCamera []
      (let [moveX (- (vx this.moveCurr) (vx this.movePrev))
            moveY (- (vy this.moveCurr) (vy this.movePrev))
            _(vec3/setFromValues moveDirection moveX moveY 0)
            angle (vec3/length moveDirection)]
        (if-not (zero? angle)
          (do
            (vec3/sub (vec3/copy this.eye this.camera.position) (.-target this))
            (vec3/normalize (vec3/copy eyeDirection this.eye))
            (vec3/normalize (vec3/copy objectUpDirection this.camera.up))
            (vec3/normalize (vec3/cross objectUpDirection eyeDirection objectSidewaysDirection))

            (vec3/setLength objectUpDirection (- (vy this.moveCurr) (vy this.movePrev) ))
            (vec3/setLength objectSidewaysDirection (- (vx this.moveCurr) (vx this.movePrev) ))

            (vec3/copy moveDirection (vec3/add objectUpDirection objectSidewaysDirection))

            (vec3/normalize (vec3/cross moveDirection this.eye axis))

            (let [angle (* angle (.-rotateSpeed this))]
              (quat/setFromAxisAngle quaternion axis angle)
              (vec3/applyQuaternion this.eye quaternion)
              (vec3/applyQuaternion this.camera.up quaternion)
              (vec3/copy (.. this -lastAxis) axis)
              (set! (.-lastAngle this) angle)))
          (do
            (set! (.-lastAngle this) (* (.-lastAngle this) (- 1.0 (.-dynamicDampingFactor this))))
            (vec3/sub (vec3/copy this.eye this.camera.position) (.-target this))
            (quat/setFromAxisAngle quaternion (.-lastAxis this) (.-lastAngle this))
            (vec3/applyQuaternion this.eye quaternion)
            (vec3/applyQuaternion this.camera.up quaternion)))
        (vec2/copy this.movePrev this.moveCurr)))))

(defn zoomCamera
  [this]
  (let [eye this.eye]
    (if (= this.state STATE.TOUCH_ZOOM_PAN)
      (let [factor (/ this.touchZoomDistanceStart this.touchZoomDistanceEnd)
            _(set! this.touchZoomDistanceStart this.touchZoomDistanceEnd)]
        (vec3/multScalar eye factor))
      (let [factor (inc (* (- (vy this.zoomEnd) (vy this.zoomStart)) this.zoomSpeed))]
        (if (and (not= 1.0 factor) (< 0.0 factor))
          (vec3/multScalar eye factor))
        (if this.staticMoving
          (vec2/copy this.zoomStart this.zoomEnd)
          (vsety this.zoomStart
                (+ (vy this.zoomStart)
                   (* (- (vy this.zoomEnd) (vy this.zoomStart)) this.dynamicDampingFactor))))))))

(defn panCamera
  [this]
  (let [mouseChange (vector2)
        objectUp (vec3/vec3)
        pan (vec3/vec3)]
    (fn panCamera []
      (vec2/sub (vec2/copy mouseChange this.panEnd) this.panStart)
      (when-not (zero? (vec3/lengthSq mouseChange))
        (vec3/multScalar mouseChange (* (vec3/length this.eye) this.panSpeed))
        (-> pan (vec3/copy this.eye) (vec3/cross this.camera.up) (vec3/setLength (vx mouseChange)))
        (->>  (-> objectUp
                (vec3/copy  this.camera.up)
                (vec3/setLength  (vy mouseChange)))
         (vec3/add pan))
        (vec3/add this.camera.position pan)
        (vec3/add this.target pan)
        (if ^boolean this.staticMoving
          (vec2/copy this.panStart this.panEnd)
          (vec2/add this.panStart
                (->
                  (vec2/sub this.panEnd this.panStart mouseChange)
                  (vec2/multScalar this.dynamicDampingFactor))))))))

(defn checkDistances
  [this]
  (let [eye this.eye]
    (when (or (not this.noZoom) (not this.noPan))
      (when (< (m/pow this.maxDistance 2) (vec3/lengthSq eye))
        (vec3/add  this.target (vec3/setLength eye this.maxDistance) this.camera.position)
        (vec2/copy this.zoomStart this.zoomEnd))
      (when (< (vec3/lengthSq eye) (m/pow this.minDistance 2))
        (vec3/add  this.target (vec3/setLength eye this.minDistance) this.camera.position)
        (vec2/copy this.zoomStart this.zoomEnd)))))

; (defn reset-trackball
;   [this]
;   (let []
;     (set! this.state STATE.NONE)
;     (set! this.prevState STATE.NONE)
;     (.copy this.target this.target0)
;     (.copy this.camera.position this.position0)
;     (.copy this.camera.up this.up0)
;     (.subVectors this.eye this.camera.position this.target)
;     (.lookAt this.camera this.target)
;     (.copy this.lastPosition this.camera.position)))

(defn mousemove [this]
  (fn [event]
    (when this.enabled
      (.preventDefault event)
      (.stopPropagation event)
      (cond
        (and (= this.state STATE.ROTATE) (not this.noRotate))
        (do
          (vec2/copy this.movePrev this.moveCurr)
          (vec2/copy this.moveCurr (getMouseOnCircle this event.pageX event.pageY)))

        (and (= this.state STATE.ZOOM) (not this.noZoom))
        (vec2/copy this.zoomEnd (getMouseOnScreen this event.pageX event.pageY))

        (and (= this.state STATE.PAN) (not this.noPan))
        (vec2/copy this.panEnd (getMouseOnScreen this event.pageX event.pageY))

        :else nil))))

(defn mouseup [this]
  (fn mouseup [event]
    (when (.-enabled this)
      (.preventDefault event)
      (.stopPropagation event)
      (set! (.-state this) STATE.NONE)
      (js/document.removeEventListener "mousemove" this.mousemove)
      (js/document.removeEventListener "mouseup" this.mouseup))))

(defn mousedown [this]
  (fn mousedown [event]
    (when this.enabled
      (.preventDefault event)
      (.stopPropagation event)
      (when (= this.state STATE.NONE)
        (set! this.state event.button))
      (cond
        (and (= this.state STATE.ROTATE) (not this.noRotate))
        (let [v (getMouseOnCircle this event.pageX event.pageY)]
          (vec2/copy this.moveCurr v)
          (vec2/copy this.movePrev this.moveCurr))

        (and (= this.state STATE.ZOOM) (not this.noZoom))
        (let [v (getMouseOnScreen this event.pageX event.pageY)]
          (vec2/copy this.zoomStart v)
          (vec2/copy this.zoomEnd this.zoomStart))

        (and (= this.state STATE.PAN) (not this.noPan))
        (let [v (getMouseOnScreen this event.pageX event.pageY)]
          (vec2/copy this.panStart v)
          (vec2/copy this.panEnd this.panStart))
        :else nil)
      (js/document.addEventListener "mousemove" this.mousemove false)
      (js/document.addEventListener "mouseup" this.mouseup false))))

(defn mousewheel [this]
  (fn mousewheel [event]
    (when-not (or (false? this.enabled)
                  (true? this.noZoom))
      (.preventDefault event)
      (.stopPropagation event)
      (case event.deltaMode
        2 (vsety this.zoomStart (- (vy this.zoomStart) (* 0.025 event.deltaY)))
        1 (vsety this.zoomStart (- (vy this.zoomStart) (* 0.01 event.deltaY)))
        (vsety this.zoomStart (- (vy this.zoomStart) (* 0.00025 event.deltaY)))))))

(defn touchstart [this]
  (fn touchstart [event]
    (when this.enabled
      (if (= 1 event.touches.length)
        (let [touch (aget event.touches 0)
              v (getMouseOnCircle this (.-pageX touch) (.-pageY touch))]
          (set! this.state STATE.TOUCH_ROTATE)
          (vec2/copy this.moveCurr v)
          (vec2/copy this.movePrev this.moveCurr))
        (let [touch0 (aget event.touches 0)
              touch1 (aget event.touches 1)
              x0 (.-pageX touch0)
              y0 (.-pageY touch0)
              x1 (.-pageX touch1)
              y1 (.-pageY touch1)
              dx (- x0 x1)
              dy (- y0 y1)
              n (m/sqrt (+ (* dx dx) (* dy dy)))
              x (/ (+ x0 x1) 2)
              y (/ (+ y0 y1) 2)]
          (set! this.state STATE.TOUCH_ZOOM_PAN)
          (set! this.touchZoomDistanceEnd n)
          (set! this.touchZoomDistanceStart n)
          (vec2/copy this.panStart (getMouseOnScreen this x y))
          (vec2/copy this.panEnd this.panStart))))))

(defn touchmove [this]
  (fn touchmove [event]
    (when this.enabled
      (.preventDefault event)
      (.stopPropagation event)
      (if (= 1 event.touches.length)
        (let [touch (aget event.touches 0)
              v (getMouseOnCircle this (.-pageX touch) (.-pageY touch))]
          (vec2/copy this.movePrev this.moveCurr)
          (vec2/copy this.moveCurr v))
        (let [touch0 (aget event.touches 0)
              touch1 (aget event.touches 1)
              x0 (.-pageX touch0)
              y0 (.-pageY touch0)
              x1 (.-pageX touch1)
              y1 (.-pageY touch1)
              dx (- x0 x1)
              dy (- y0 y1)
              n (m/sqrt (+ (* dx dx) (* dy dy)))
              x (/ (+ x0 x1) 2)
              y (/ (+ y0 y1) 2)]
          (set! this.touchZoomDistanceEnd n)
          (vec2/copy this.panEnd (getMouseOnScreen this x y)))))))

(defn touchend [this]
  (fn touchend [event]
    (when this.enabled
      (case event.touches.length
        0 (set! this.state STATE.NONE)
        1 (let [touch (aget event.touches 0)
                v (getMouseOnCircle this (.-pageX touch) (.-pageY touch))]
            (set! this.state STATE.TOUCH_ROTATE)
            (vec2/copy this.moveCurr v)
            (vec2/copy this.movePrev this.moveCurr))
        nil))))

(def EPS 0.000001)

(defn update-trackball [render-fn]
  (fn update-trackball [this]
    (let [eye this.eye]
      (vec3/sub this.camera.position this.target eye)
      (if-not this.noRotate
        (.rotateCamera this))
      (if-not this.noZoom
        (.zoomCamera this))
      (if-not this.noPan
        (.panCamera this))
      (vec3/add this.target eye this.camera.position)
      (.checkDistances this)
      (.lookAt this.camera this.target)
      (when (< EPS (vec3/distanceSq this.lastPosition this.camera.position))
        (render-fn)
        (vec3/copy this.lastPosition this.camera.position)))))

(defn dispose [this]
  (this.domElement.removeEventListener "contextmenu", this.contextmenu, false)
  (this.domElement.removeEventListener "mousedown", this.mousedown, false)
  (this.domElement.removeEventListener "wheel", this.mousewheel, false)
  (this.domElement.removeEventListener "touchstart", this.touchstart, false)
  (this.domElement.removeEventListener "touchend", this.touchend, false)
  (this.domElement.removeEventListener "touchmove", this.touchmove, false)
  (js/document.removeEventListener "mousemove", this.mousemove, false)
  (js/document.removeEventListener "mouseup", this.mouseup, false)
  (js/window.removeEventListener "keydown", this.keydown, false)
  (js/window.removeEventListener "keyup", this.keyup, false)
  (doseq [k (array-seq (goog.object.getKeys this))]
    (js-delete this k)))

(defn TrackballControls
  [camera node render-fn]
  (let [o  #js{}
        update-fn (update-trackball render-fn)] ;; <================
    (set! o.rotateCamera (rotateCamera o))
    (set! o.panCamera (panCamera o))
    ;;;
    (set! o.mousewheel (mousewheel o))
    (set! o.mousedown  (mousedown o))
    (set! o.mouseup    (mouseup o))
    (set! o.mousemove  (mousemove o))
    (set! o.touchstart (touchstart o))
    (set! o.touchmove  (touchmove o))
    (set! o.touchend   (touchend o))
    (set! o.contextmenu (fn [event] (when (.-enabled o) (.preventDefault event))))
    ;;props
    (set! o.state STATE.NONE)
    (set! o.prevState STATE.NONE)

    (set! o.camera camera)
    (set! o.domElement node)
    (set! o.screen #js{"left" 0 "top" 0 "width" 0 "height" 0})

    (set! o.eye          (vec3/vec3))
    (set! o.target       (vec3/vec3))
    (set! o.lastPosition (vec3/vec3))
    (set! o.lastAxis     (vec3/vec3))

    (set! o.movePrev  (vector2))
    (set! o.moveCurr  (vector2))
    (set! o.panEnd    (vector2))
    (set! o.panStart  (vector2))
    (set! o.zoomEnd   (vector2))
    (set! o.zoomStart (vector2))

    (set! o.enabled true)
    (set! o.noRotate false)
    (set! o.noZoom false)
    (set! o.noPan false)
    (set! o.staticMoving false)

    (set! o.rotateSpeed 1.0)
    (set! o.zoomSpeed 1.2)
    (set! o.panSpeed 0.3)
    (set! o.dynamicDampingFactor 0.2)
    (set! o.minDistance 0)
    (set! o.maxDistance ##Inf)
    (set! o.lastAngle 0)
    (set! o.touchZoomDistanceStart 0)
    (set! o.touchZoomDistanceEnd 0)

    (specify! o
              Object
              (dispose [this] (dispose this))
              (update [this] (update-fn this))
              (zoomCamera [this] (zoomCamera this))
              (checkDistances [this] (checkDistances this))
              (handleResize [this] (handleResize this)))
    (.addEventListener node "wheel" o.mousewheel #js{"capture" true "passive" false})
    (.addEventListener node "mousedown" o.mousedown #js{"capture" true "passive" false})
    (.addEventListener node "touchstart" o.touchstart #js{"capture" true "passive" false})
    (.addEventListener node "touchmove" o.touchmove #js{"capture" true "passive" false})
    (.addEventListener node "touchend" o.touchend #js{"capture" true "passive" false})
    (.addEventListener node "contextmenu" o.contextmenu #js{"capture" true "passive" false}) ;;;;;;<---
    (.handleResize o)
    ; (.update o)
    o))

