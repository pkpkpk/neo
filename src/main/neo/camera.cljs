(ns neo.camera
  (:require [neo.math :as m :include-macros true]
            [neo.vec.mat4 :as mat4]
            [neo.object3D :as o3D :refer [object3D]]))

(defn camera-updateMatrixWorld
  [camera force]
  (do
    (o3D/updateMatrixWorld camera force)
    (mat4/invert (.. camera -matrixWorld) (.. camera -matrixWorldInverse))))

(defn setFocalLength ;focalLength 35 vExtentSlope 0.48403707518022654 fov' 51.657406354011556
  "Sets the FOV by focal length in respect to the current .filmGauge.
   The default film gauge is 35, so that the focal length can be specified for
   a 35mm (full frame) camera.
   Values for focal length and film gauge must have the same unit.
   see http://www.bobatkins.com/photography/technical/field_of_view.html"
  [this focalLength]
  (let [vExtentSlope (* 0.5 (.getFilmHeight this) (/ 1 focalLength))
        fov' (* m/rad2deg 2 (m/atan vExtentSlope))]
    (set! (.-fov this) fov')
    (.updateProjectionMatrix this)))

(defn getFocalLength
  "Calculates the focal length from the current .fov and .filmGauge."
  [this]
  (let [vExtentSlope (m/tan (* m/deg2rad 0.5 (.-fov this)))]
    (* 0.5 (/ (.getFilmHeight this) vExtentSlope))))

(defn getEffectiveFOV [this]
  (* m/rad2deg 2 (m/atan (/ (m/tan (* m/deg2rad 0.5 (.-fov this))) (.-zoom this)))))

(defn getFilmWidth
  "film not completely covered in landscape format (aspect > 1)"
  [this]
  (* (.-filmGauge this) (min (.-aspect this) 1)))

(defn getFilmHeight
  "film not completely covered in landscape format (aspect > 1)"
  [this]
  (/ (.-filmGauge this) (max (.-aspect this) 1)))

(defn updateProjectionMatrix [this]
  (let [near (.-near this)
        top (atom (* near (m/tan (* m/deg2rad 0.5 (.-fov this))) (/ 1 (.-zoom this))))
        height (atom (* 2 @top))
        width (atom  (* @height (.-aspect this)))
        left (atom (* -0.5 @width))]
    ; (when (and (.-view this) (.. this -view -enabled)) ...)
    (when-not (zero? (.-filmOffset this))
      (swap! left + (* near (.-filmOffset this) (/ 1 (.getFilmWidth this)))))
    (mat4/makePerspective (.. this -projectionMatrix)
                      @left
                      (+ @left @width)
                      @top
                      (- @top @height)
                      near
                      (.-far this))))

(defn getCameraCSSMatrix [fov widthHalf heightHalf m]
  (let []
    (assert (every? #(not (js/isNaN %)) (array-seq m)) "camera matrix contains NaNs!")
    (aset m  1 (- (aget m 1)))
    (aset m  5 (- (aget m 5)))
    (aset m  9 (- (aget m 9)))
    (aset m 13 (- (aget m 13)))
    (str "translateZ(" fov "px)" (mat4/->CSS m) "translate(" widthHalf "px," heightHalf "px)")))

(defn perspective-camera
  ([](perspective-camera nil nil nil nil))
  ([fov, aspect, near, far]
   (let [fov (or fov 50)
         aspect (or aspect 1)
         near (or near 0.1)
         far (or far 2000)
         o (object3D)
         matrixWorldInverse (mat4/mat4)
         projectionMatrix (mat4/mat4)
         zoom 1
         focus 10
         filmGauge 35
         filmOffset 0]
     (set! (.-matrixWorldInverse o) matrixWorldInverse)
     (set! (.-projectionMatrix o) projectionMatrix )
     (set! (.-fov o) fov)
     (set! (.-zoom o) zoom)
     (set! (.-near o) near)
     (set! (.-far o) far)
     (set! (.-focus o) focus)
     (set! (.-aspect o) aspect)
     (set! (.-filmGauge o) filmGauge)
     (set! (.-filmOffset o) filmOffset)
     (set! (.-isCamera o) true)
     (specify! o
       Object
       (updateMatrixWorld [this force] (camera-updateMatrixWorld this force))
       (setFocalLength [this focalLength] (setFocalLength this focalLength))
       (getFocalLength [this] (getFocalLength this))
       (getEffectiveFOV [this] (getEffectiveFOV this))
       (getFilmWidth [this] (getFilmWidth this))
       (getFilmHeight [this] (getFilmHeight this))
       (updateProjectionMatrix [this](updateProjectionMatrix this)))
     (.updateProjectionMatrix o)
     o)))

