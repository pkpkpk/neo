(ns neo.object3D
  (:require [neo.math :as m :include-macros true]
            [neo.vec.vec3 :as vec3]
            [neo.vec.euler :as euler]
            [neo.vec.mat4 :as mat4]
            [neo.vec.quaternion :as quat :refer [quaternion]]))

(defn log [& args] (.apply js/console.log js/console (into-array args)))

(def default-up (vec3/vec3 0 1 0))
(def DefaultMatrixAutoUpdate true)
(def object-id (atom 0))

(defn lookAt [o v] ; or x,y,z
	; This method does not support objects with rotated and/or translated parent(s)
  (let [m (mat4/mat4)
        v (vec3/vclone v)] ; when xyz, v.set(x y z)
    (if (.-isCamera o)
      (mat4/lookAt m (.-position o) v (.-up o))
      (mat4/lookAt m v (.-position o) (.-up o)))
    (quat/setFromRotationMatrix (.-quaternion o) m)))

(defn updateMatrix [o]
  (mat4/compose (.. o -matrix) (.-position o) (.-quaternion o) (.-scale o))
  (set! (.-matrixWorldNeedsUpdate o) true))

(defn updateMatrixWorld
  ([o](updateMatrixWorld o false))
  ([o force]
   (do
     (when (.-matrixAutoUpdate o) (updateMatrix o))
     (when (or (.-matrixWorldNeedsUpdate o) force)
       (if (nil? (.-parent o))
         (mat4/copy (.. o -matrixWorld) (.. o -matrix))
         (mat4/mult (.. o -parent -matrixWorld) (.. o -matrix) (.. o -matrixWorld)))
       (set! (.-matrixWorldNeedsUpdate o) false)))))

(def getWorldQuaternion
  (let [position (vec3/vec3)
        scale (vec3/vec3)]
    (fn getWQ
      ([o](getWQ o (quat/quaternion)))
      ([o target]
       (updateMatrixWorld o)
       (mat4/decompose (.-matrixWorld o) position target scale)
       target))))

(def getWorldPosition
  (let []
    (fn getWP
      ([o](getWP o (vec3/vec3)))
      ([o target]
       (updateMatrixWorld o true)
       (vec3/setFromMatrixPosition target (.-matrixWorld o))))))

(def getWorldDirection
  (let [q (quat/quaternion)]
    (fn getWD
      ([o](getWD o (vec3/vec3)))
      ([o target]
       (getWorldQuaternion o q)
       (vec3/applyQuaternion (vec3/setFromValues target 0 0 1) q)))))

(def getWorldScale
  (let [position (vec3/vec3)
        q (quat/quaternion)]
    (fn getWS
      ([o](getWS o (vec3/vec3)))
      ([o target]
       (updateMatrixWorld o)
       (mat4/decompose (.-matrixWorld o) position q target)
       target))))

(defn applyMatrix [o m]
  (mat4/mult (.-matrix o) m)
  (mat4/decompose (.-matrix o) (.-position o) (.-quaternion o) (.-scale o)))

(defn applyQuaternion [o q]
  (quat/premult (.-quaternion o) q)
  o)

(defn setRotationFromAxisAngle [o axis angle]
  (quat/setFromAxisAngle (.-quaternion o) axis angle)
  o)

(defn setRotationFromEuler [o e]
  (quat/setFromEuler (.-quaternion o) e true))

(defn setRotationFromMatrix [o m]
  ;;assuming mat is unscaled
  (quat/setFromRotationMatrix (.-quaternion o) m))

(defn setRotationFromQuaternion [o q]
  ;;assumes q is normalized
  (quat/copy (.-quaternion o) q))

(def rotateOnAxis
  ; rotate object on axis in object space
  ; axis is assumed to be normalized
  (let [q (quat/quaternion)]
    (fn [o axis angle]
      (quat/setFromAxisAngle q axis angle)
      (quat/mult (.-quaternion o) q)
      o)))

(def rotateOnWorldAxis
  ; rotate object on axis in world space
  ; axis is assumed to be normalized
  ; method assumes no rotated parent
  (let [q (quat/quaternion)]
    (fn [o axis angle]
      (quat/setFromAxisAngle q axis angle)
      (quat/premult (.-quaternion o) q)
      o)))

(def rotateX
  (let [x-axis (vec3/vec3 1 0 0)]
    (fn [o angle]
      (rotateOnAxis o x-axis angle))))

(def rotateY
  (let [y-axis (vec3/vec3 0 1 0)]
    (fn [o angle]
      (rotateOnAxis o y-axis angle))))

(def rotateZ
  (let [z-axis (vec3/vec3 0 0 1)]
    (fn [o angle]
      (rotateOnAxis o z-axis angle))))


(def translateOnAxis
  ; translate object by distance along axis in object space
  ; axis is assumed to be normalized
  (let [v (vec3/vec3)]
    (fn [o axis distance]
      (vec3/applyQuaternion (vec3/copy v axis) (.-quaternion o))
      (vec3/add (.-position o) (vec3/multScalar v distance))
      o)))

(def translateX
  (let [x-axis (vec3/vec3 1 0 0)]
    (fn [o distance]
      (translateOnAxis o x-axis distance))))

(def translateY
  (let [y-axis (vec3/vec3 0 1 0)]
    (fn [o distance]
      (translateOnAxis o y-axis distance))))

(def translateZ
  (let [z-axis (vec3/vec3 0 0 1)]
    (fn [o distance]
      (translateOnAxis o z-axis distance))))

(defn ->CSS [o]
  (let [m (.-matrixWorld o)]
    (aset m 4 (- (aget m 4)))
    (aset m 5 (- (aget m 5)))
    (aset m 6 (- (aget m 6)))
    (aset m 7 (- (aget m 7)))
    (str "translate(-50%,-50%)" (mat4/->CSS m))))

(defn add-child [this o]
  (assert (not (identical? this o)))
  (do
    (when-not (nil? (.-parent o))
      (.remove-child (.-parent o) o))
    (set! (.-parent o) this)
    (.push (.-children this) o) ;.. dispatch 'added'
    this))

(defn remove-child [this o]
  (let [index (.indexOf (.-children this) o)]
    (when-not (= -1 index)
      (set! (.-parent o) nil)
      (.splice (.-children this) index 1)) ;.. dispatch 'removed'
    this))

(defn traverse [o cb]
  (cb o)
  (run! #(traverse % cb) (.-children o)))

(defn traverseVisible
  [o cb]
  (when (.-visible o)
    (cb o)
    (run! #(traverseVisible % cb) (.-children o))))

(defn traverseAncestors
  [o cb]
  (when-let [parent (.-parent o)]
    (cb parent)
    (traverseAncestors parent cb)))

(deftype Object3D
  [id parent children up
   position rotation quaternion scale
   matrix matrixWorld
   ; normalMatrix modelViewMatrix ;>>??????????
   matrixAutoUpdate matrixWorldNeedsUpdate
   visible frustumCulled]
  Object
  (add-child [this child] (add-child this child))
  (remove-child [this child](remove-child this child))
  (updateMatrixWorld [this force](updateMatrixWorld this force))
  (lookAt [this v](lookAt this v))
  (updateMatrix [this](updateMatrix this)))

(defn object3D []
  (let [id (swap! object-id inc)
        position (vec3/vec3)
        rotation (euler/euler)
        quaternion (quaternion)
        scale (vec3/vec3 1 1 1)
        up  (vec3/vclone default-up)
        parent nil
        children #js[]
        onRotationChange #(quat/setFromEuler quaternion rotation false)
        _(.onChange rotation onRotationChange)
        onQuaternionChange #(euler/setFromQuaternion rotation quaternion js/undefined false)
        _(.onChange quaternion onQuaternionChange)
        matrix (mat4/mat4)
        matrixWorld (mat4/mat4)
        ; normalMatrix (matrix4) ;?????
        ; modelViewMatrix (matrix4)
        matrixAutoUpdate DefaultMatrixAutoUpdate ;??/
        matrixWorldNeedsUpdate false
        visible true
        frustumCulled true]
    (Object3D. id parent children up
               position rotation quaternion scale
               matrix matrixWorld
               ; normalMatrix modelViewMatrix ;>>??????????
               matrixAutoUpdate matrixWorldNeedsUpdate
               visible frustumCulled)))

;; Props::
; id/uuid, layers, renderOrder, userData
;;cbs
; onBeforeRender, onAfterRender
; Methods:
; copy, clone, to-json,
; getObjectById, getObjectByNasme, getObjectByProperty
; worldToLocal localToWorld

