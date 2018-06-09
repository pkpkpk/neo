(ns periodic.core
  (:require-macros [neo.macros :refer [vx vy vz vsetx vsety vsetz]])
  (:require [neo.math :refer [pi -pi] :refer-macros [abs acos sqrt floor] :as m]
            [neo.core :as neo]
            [neo.vec.vec3 :as vec3]
            [neo.geometry :refer [spherical cylindrical]]
            [neo.camera :as cam :refer [perspective-camera]]
            [neo.object3D :as o3d :refer [object3D]]
            [neo.controls.trackball :refer [TrackballControls]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]
            [periodic.table :refer [table]]))

(enable-console-print!)

(defn table-object3D
  [{:keys [col row]}]
  (let [o (object3D)]
    (vsetx (.-position o) (- (* col 140) 1330) )
    (vsety (.-position o) (- (- (* row 180) 990)) )
    o))

(defn sphere-object3D
  [{:keys [col row i]} v spherical]
  (let [l (count table)
        phi (acos (+ (- 1) (/ (* 2 i) l)))
        theta (* (sqrt (* l pi)) phi)
        o (object3D)
        _(.set spherical 800 phi theta)
        _(vec3/setFromSpherical (.-position o)  spherical)
        _(vec3/multiplyScalar (vec3/copy v (.-position o)) 2)
        _(.lookAt o v)]
    o))

(defn helix-object3D
  [{:keys [col row i]} v cylindrical]
  (let [l (count table)
        theta (+ (* i 0.175 ) pi)
        o (object3D)
        _(.set cylindrical 900 theta (+ (- (* i 8)) 450))
        _(vec3/setFromCylindrical (.-position o)  cylindrical)
        _(vsetx v (* 2 (vx (.-position o))))
        _(vsety v (vy (.-position o)))
        _(vsetz v (* 2 (vz (.-position o))))
        _(.lookAt o v)]
    o))

(defn grid-object3D
  [{:keys [col row i]}]
  (let [o (object3D)
        _(vsetx (.-position o) (- (* (mod i 5) 400) 800))
        _(vsety (.-position o) (+ (- (* (mod (floor (/ i 5)) 5) 400)) 800))
        _(vsetz (.-position o) (- (* (floor (/ i 25)) 1000) 2000))]
    o))

(defn animate-object [owner key duration]
  (let [target (om/get-state owner key)
        o (om/get-state owner :object)]
    (neo/tween! (om/get-state owner :position)
      {:duration (+ (* (rand) duration) duration)
       ; :curve (get neo/curves :in-out-expo)
       :curve (get neo/curves :out-back)
       :to (.. target -position) })
    (neo/tween! (om/get-state owner :rotation)
      {:duration (+ (* (rand) duration) duration)
       ; :curve (get neo/curves :in-out-expo) ;:out-back
       :curve (get neo/curves :out-back)
       :to #js{:x (.. target -rotation -x) ;; rotation is an euler, not vec
               :y (.. target -rotation -y)
               :z (.. target -rotation -z)}
       :onUpdate #(.onChangeCallback (.-rotation o))})))

(def cache #js{:camera #js{:fov 0 :style ""}})

(defonce elements #js[])

(defn animate! [k]
  (let [duration 2000]
    (run! #(animate-object % k duration) elements)))

(defui Element
  Object
  (renderObject3D [this]
    (let [o (om/get-state this :object)
          _(.updateMatrixWorld o)
          style (o3d/->CSS o)]
      (set! (.. o -node -style -WebkitTransform) style)
      (set! (.. o -node -style -MozTransform) style)
      (set! (.. o -node -style -transform) style)))
  (updateMatrixWorld [this](.updateMatrixWorld (om/get-state this :object)))
  (componentWillUnmount [this] (neo/deregister-owner this))
  (componentDidMount [this]
    (let [o (let [o (object3D)]
              (vsetx (.. o -position) (- (* (rand) 4000) 2000))
              (vsety (.. o -position) (- (* (rand) 4000) 2000))
              (vsetz (.. o -position) (- (* (rand) 4000) 2000))
              (set! (.-node o) (dom/node this))
              o)
          rot (neo/ObjectTweener. (.-rotation o))
          pos (neo/ArrayTweener. (.-position o))
          props (om/props this)
          state {:object o
                 :rotation rot
                 :position pos
                 :table (table-object3D props)
                 :grid (grid-object3D props)
                 :sphere (sphere-object3D props (vec3/vec3) (spherical))
                 :helix (helix-object3D props (vec3/vec3) (cylindrical))}]
      (neo/register-owner this [rot pos]) ;add tweeners to dirty checker
      (om/set-state! this state)
      (.push elements this)))
  (render [this]
    (let [{:keys [i letter name weight col row color]} (om/props this)
          transform (when-let [o (om/get-state this :object)]
                      (.updateMatrixWorld o)
                      (o3d/->CSS o))]
      (dom/div #js{:className "element"
                   :style #js{:position "absolute"
                              :backgroundColor (str "rgba(" color ","  (+ 0.25 (* (rand) 0.5)) ")")
                              :WebkitTransform transform
                              :MozTransform transform
                              :transform transform}}
        (dom/div #js{:className "number"} i)
        (dom/div #js{:className "symbol"} letter)
        (dom/div #js{:className "details"} name (dom/br nil) weight)))))

(def element (om/factory Element {:keyfn :i}))

(defui RootView
  "By default neo will call component.forceUpdate()...for low volume react render
   methods are fine, but if you are just changing the transform, no reason to do
   all that work every frame. render-object3d & render-scene only touch the transforms on
   their respective components"
  Object
  (initLocalState [this]{:listeners []})
  (componentWillUnmount [this]
    (doseq [[target event cb] (om/get-state this :listeners)]
      (.removeEventListener target event cb))
    (.dispose (om/get-state this :controls))
    (neo/clear-all!)
    (goog.array.clear elements))
  (setSize [this width height]
    (let [renderElement (dom/node this "renderer")
          cameraElement (dom/node this "camera")]
      (om/update-state! this assoc
                        :width width
                        :height height
                        :widthHalf (/ width 2)
                        :heightHalf (/ height 2)   )
      (set! (.. renderElement -style -width) (str width "px"))
      (set! (.. renderElement -style -height) (str height "px"))
      (set! (.. cameraElement -style -width) (str width "px"))
      (set! (.. cameraElement -style -height) (str height "px"))))
  (get-transform [this] ;> [renderer-perspective-string camera-transform-string]
    (when-let [camera (om/get-state this :camera)]
      (let [widthHalf (om/get-state this :widthHalf)
            heightHalf (om/get-state this :heightHalf)
            _(assert (and (number? widthHalf) (number? heightHalf)))
            fov (* (aget (.. camera -projectionMatrix) 5) heightHalf)
            m (.. camera -matrixWorldInverse)]
        (when-not (.-parent camera) (.updateMatrixWorld camera))
        [fov (cam/getCameraCSSMatrix fov widthHalf heightHalf m)])))
  (render-scene [this] ; this only changes transform on the camera, objects are run by the dirty checker
    (when-let [[fov style] (.get-transform this)]
      (when (not= fov (.. cache -camera -fov))
        (let [s (str fov "px")
              renderElement (dom/node this "renderer")]
          (set! (.. renderElement -style -WebkitPerspective) s)
          (set! (.. renderElement -style -MozPerspective) s)
          (set! (.. renderElement -style -perspective) s)
          (set! (.. cache -camera -fov) fov)))
      (when (not= style (.. cache -camera -style))
        (let [cameraElement (dom/node this "camera")]
          (set! (.. cameraElement -style -WebkitTransform) style)
          (set! (.. cameraElement -style -MozTransform) style)
          (set! (.. cameraElement -style -transform) style)))))
  (componentDidMount [this]
    (let [camera (perspective-camera 40 (/ js/window.innerWidth js/window.innerHeight) 1 10000)
          _(vsetz (.-position camera) 3000)
          _(.setSize this js/window.innerWidth js/window.innerHeight) ; camera needs dimensions before a render can occur
          onWindowResize (fn []
                           (set! (.-aspect camera) (/ js/window.innerWidth js/window.innerHeight))
                           (.updateProjectionMatrix camera)
                           (.setSize this js/window.innerWidth js/window.innerHeight)
                           (.render-scene this))
          controls (TrackballControls camera (dom/node this "renderer")
                      (fn render-scene [](.render-scene this)))]
      (om/update-state! this merge {:listeners [[js/window "resize" onWindowResize]]
                                    :camera camera
                                    :controls controls})
      (js/window.addEventListener "resize" onWindowResize false)
      (set! (.-rotateSpeed controls) 0.5)
      (set! (.-minDistance controls) 500)
      (set! (.-maxDistance controls) 6000)
      ; Here we drive the camera transform with the trackball controls, but the
      ; element transforms are driven by the dirty-checker
      (set! neo/*render-fn* (fn renderO3D [o](.renderObject3D o)))
      (neo/every-tick #(.update controls))
      (neo/start!)))
  (render [this]
    (let [[fov transform] (.get-transform this)
          perspective (str fov "px")]
      (dom/div nil
        (dom/div #js{:ref "renderer"
                     :style #js{:overflow "hidden"
                                :position "absolute"
                                :WebkitPerspective perspective
                                :MozPerspective perspective
                                :perspective perspective}}
          (dom/div #js{:ref "camera"
                       :style #js{:WebkitTransformStyle "preserve-3d"
                                  :WebkitTransform transform
                                  :MozTransform transform
                                  :transform transform}}
                   (map element table)))
        (dom/div #js{:id "menu"}
          (for [k [:table :sphere :helix :grid]]
            (dom/button #js{:key k :id k :onClick #(animate! k)} (str k))))))))

(def reconciler (om/reconciler {:state {} :logger false}))

(om/add-root! reconciler RootView (gdom/getElement "app"))

