(ns neo.core
  (:require [neo.math :as math :include-macros true
             :refer [pi sin cos asin pow sqrt]])
  (:import [goog.dom ViewportSizeMonitor]))

(when (exists? js/window)
  (defonce vp-monitor (ViewportSizeMonitor.)))

(defn get-vp []
  (let [vp (.getSize vp-monitor)]
    [(.-width vp) (.-height vp)]))

(defn get-screen []
  [(.-width (.-screen js/window)) (.-height (.-screen js/window))])

(defn size [node]
  (let [cr (.getBoundingClientRect node)]
    [(.-width cr) (.-height cr)]))

(defn clientRect [node]
  (let [m (.getBoundingClientRect node)
        top (.-top m)
        bottom (.-bottom m)
        left (.-left m)
        right (.-right m)
        w     (.-width m)
        h     (.-height m)]
    {:top top
     :bottom bottom
     :mid-y (+ bottom (/ (- top bottom) 2))
     :left left
     :right right
     :mid-x (+  left (/ w 2))
     :h h
     :w w}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- out-bounce [t]
  (cond
    (< t (/ 1 2.75))   (* t t 7.5625)
    (< t (/ 2 2.75))   (+ 0.75  (* 7.5625 (- t (/ 1.5 2.75)) (- t (/ 1.5 2.75))))
    (< t (/ 2.5 2.75)) (+ 0.9375  (* 7.5625 (- t (/ 2.25 2.75)) (- t (/ 2.25 2.75))))
    :default (+ 0.984375  (* 7.5625 (- t (/ 2.625 2.75)) (- t (/ 2.625 2.75))))))

(defn- in-bounce [t](- 1 (out-bounce (- 1 t))))

(def curves {
  :linear           identity
  :ease-in          (fn [t] (* t t))
  :ease-out         (fn [t] (* t (- 2 t)))
  :ease-in-out      (fn [t] (if ^boolean (<= t 0.5)(* 2 t t)(+ (- 1) (* t t (- 2)) (* 4 t))))
  :ease-out-bounce  (fn [t] (* t (- 3 (* 2 t))))
  :spring           (fn [t] (+ t (* (- 1 t) (sin (* t pi 1.25)))))
  :in-quad          (fn [t] (* t t))
  :out-quad         (fn [t] (+ (* (- 1) (dec t) (dec t)) 1))
  :in-out-quad      (fn [t] (let [x  (/ t 0.5)]
                              (if ^boolean (< x 1)
                                (* x x .5)
                                (* (- 0.5) (+ (- 1) (* (dec x) (- (dec x) 2)))))))
  :in-cubic         (fn [t] (* t t t))
  :out-cubic        (fn [t] (+ 1 (pow (dec t) 3)))
  :in-out-cubic     (fn [t] (let [x (/ t 0.5)]
                              (if ^boolean (< x 1)
                                (* 0.5 x x x)
                                (* 0.5 (+ 2 (pow (- x 2) 3))))))
  :in-quart         (fn [t] (* t t t t))
  :out-quart        (fn [t] (- (+ (- 1) (pow (dec t) 4))))
  :in-out-quart     (fn [t] (let [x (/ t 0.5)]
                              (if ^boolean (< x 1)
                                (* 0.5 x x x x)
                                (* 0.5 (+ (- 2) (pow (- x 2) 4))))))
  :in-quint         (fn [t] (* t t t t t))
  :out-quint        (fn [t] (+ 1 (pow (dec t) 5)))
  :in-out-quint     (fn [t] (let [x (/ t 0.5)]
                              (if ^boolean (< x 1)
                                (* 0.5 x x x x x)
                                (* 0.5 (+ 2 (pow (- x 2) 5))))))
  :in-sine          (fn [t] (+ 1 (* (- 1) (cos (* t (/ pi 2))))))
  :out-sine         (fn [t] (sin (* t (/ pi 2))))
  :in-out-sine      (fn [t] (* (- 0.5) (+ (- 1) (cos (* t pi)))))
  :in-expo          (fn [t] (if ^boolean (zero? t) 0 (pow 2 (* 10 (dec t)))))
  :out-expo         (fn [t] (if ^boolean (= t 1) 1 (+ 1 (- (pow 2 (* t (- 10)))))))
  :in-out-expo      (fn [t] (cond
                              (zero? t) 0
                              (= 1 t) 1
                              (< (/ t 0.5) 1) (* 0.5 (pow 2 (* 10 (dec (/ t 0.5)))))
                              :default (* 0.5 (+ 2 (- (pow 2 (* (- 10) (dec (/ t 0.5)))))))))
  :in-circ          (fn [t] (- (- (sqrt (- 1 (* t t))) 1))); [0-1]
  :out-circ         (fn [t] (sqrt (- 1 (* (dec t) (dec t))))) ;[0 2]
  :in-out-circ      (fn [t] (let [x (/ t .5)]
                              (if ^boolean (< x 1)
                                (* (- 0.5) (- (sqrt (- 1 (* x x))) 1))
                                (* 0.5 (+ (sqrt (- 1 (pow (- x 2) 2))) 1)))))
  :in-elastic       (fn [t] (let [p 0.3
                                  s (* (/ p (* 2 pi)) (asin 1))]
                              (cond
                                (= t 0) 0
                                (= t 1) 1
                                :default (* (- (pow 2 (* 10 (dec t)))) (sin (/ (* (- (dec t) s) (* 2 pi)) p))))))
  :out-elastic      (fn [t] (let [p 0.3
                                  s (* (/ p (* 2 pi)) (asin 1))]
                              (cond
                                (= t 0) 0
                                (= t 1) 1
                                :default (+ 1 (* (pow 2 (* (- 10) t)) (sin (/ (* (- t s) (* 2 pi)) p)))))))
  :in-out-elastic   (fn [t] (let [p (* 0.3 1.5)
                                  s (* (/ p (* 2 pi)) (asin 1.0))
                                  x (/ t 0.5)]
                              (cond
                                (= t 0) 0
                                (= x 2) 1
                                (< x 1) (* (- 0.5) (pow 2 (* 10 (dec x))) (sin (* (- (dec x) s) (/ (* 2 pi) p))))
                                :default (+ 1.0
                                            (* 0.5 (pow 2 (*(- 10)(dec x)))  (sin (* (- (dec x) s) (* 2 (/ pi p)))))))))
  :in-back          (fn [t]
                      (let [s 1.70158]
                        (* t t (- (* t (inc s)) s))))
  :out-back         (fn [t]
                     (let [s 1.70158]
                       (+ 1 (* (dec t) (dec t) (+ s (* (dec t) (inc s)))))))
  :in-out-back      (fn [t]
                      (let [s  (* 1.525 1.70158)
                            x  (/ t 0.5)]
                        (if ^boolean (< x 1)
                          (* 0.5 x x (- (* x (inc s)) s))
                          (* 0.5 (+ 2 (* (- x 2)(- x 2) (+ (* (inc s) (- x 2)) s)))))))
  :out-bounce       out-bounce
  :in-bounce        in-bounce
  :in-out-bounce    (fn [t]
                      (if ^boolean (< t 0.5)
                        (* 0.5 (in-bounce (* t 2)))
                        (+ 0.5 (* 0.5 (out-bounce (- (* t 2) 1.0))))))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def now
  (if (exists? js/performance)
    (js/performance.now.bind js/performance)
    (js/Date.now.bind js/Date)))

(defn- calc-velocity
  [current start curve duration t]
  (let [eps   1e-7
        speed (/ (- (curve t) (curve (- t eps))) eps)]
    (if (vector? current)
      (let [velocity #js[]]
        (doseq [[c s] (map vector current start)]
          (if ^boolean (number? c)
            (.push velocity (/ (* speed (- c s)) duration))
            (.push velocity 0)))
        (vec velocity))
      (/ (* speed (- current start)) duration))))

(defn- calc-state
  [start end t]
  (if (vector? start) ;; see interpol.js, tweenjs array interpolation examples
    (let [state #js[]]
      (doseq [[s e] (map vector start end)]
        (if (number? s)
          (.push state (goog.math.lerp s e t))
          (.push state s)))
      (vec state))
    (goog.math.lerp start end t)))

(defprotocol ITweener
  (set-value! [this v] "manually change the value")
  (tween! [this object-transiton-map]
          [this start end transiton-map] "trigger a state transition")
  (update-state [this] "private fn called internally on each raf pass"))

(defn start-tweening! [this transition]
  (let [curve (let [c (get transition :curve)]
                (if (fn? c)
                  c
                  (or (get curves c)
                      (do
                        (when ^boolean goog.DEBUG
                          (js/console.warn "invalid or missing :curve " (pr-str c) ", defaulting to linear"))
                        identity))))
        transition (assoc transition :curve curve)
        setup #(do
                (set! (.-onStart this) (get transition :onStart))
                (set! (.-onUpdate this) (get transition :onUpdate))
                (set! (.-onFinish this) (get transition :onFinish))
                (set! (.-onCancel this) (get transition :onCancel))
                ; (!set this.valuesStartRepeat ...) ;clone object
                ; (set! (.-velocity this) nil)
                ; interpolationFunction
                (set! (.-start-time this) (now))
                (set! (.-end-val this) (get transition :to))
                (set! (.-start-val this) (get transition :from @this))
                (set! (.-active? this) true)
                (set! (.-dirty? this) true)
                (set! (.-transition this) transition)
                (update-state this)
                (if (some? this.onStart)
                  (.onStart this @this)))]
       (if-let [t (get transition :delay)]
         (js/setTimeout setup t)
         (setup))))

(defn- ^boolean tween-num-state
  "mutates the tweener state appropriate to place in time"
  [twnr]
  (let [transition (.-transition twnr)
        start-val  (.-start-val twnr)
        start-time (.-start-time twnr)
        value      (.-v twnr)
        end-val    (.-end-val twnr)
        curve      (get transition :curve)
        duration   (get transition :duration)
        time-since-start (- (now) start-time)] ;;should we use same time across frame/update-cycle?
    (if (< time-since-start duration)
      (let [t        (/ time-since-start duration)
            newval   (calc-state start-val end-val (curve t))
            #_ #_ velocity (calc-velocity value start-val curve duration t)]
        (set! (.-v twnr) newval)
        #_(set! (.-velocity twnr) velocity)
        true)
      (let [#_#_ velocity (calc-velocity end-val start-val curve duration 1)]
        (set! (.-v twnr) end-val)
        #_(set! (.-velocity twnr) velocity)
        false))))

(defn ^boolean tween-object-state
  [this]
  (let [transition (.-transition this)
        start-val  (.-start-val this)
        start-time (.-start-time this)
        end-val    (.-end-val this)
        curve      (get transition :curve)
        duration   (get transition :duration)
        time-since-start (- (now) start-time)]
    (if (< time-since-start duration)
      (let [t        (/ time-since-start duration)]
        (doseq [k (goog.object.getKeys end-val)]
          (when-not (undefined? (goog.object.get start-val k))
            (let [start (or (goog.object.get start-val k) 0)
                  end (goog.object.get end-val k)]
              (if (array? end)
                nil ; (goog.object.set this.object k (.interpolationFunction this end value))
                (goog.object.set this.v k (goog.math.lerp start end (curve t)))))))
        true)
      ; (if (< 0 this.repeat)
      ;   (when (js/isFinite this.repeat)
      ;     (set! this.repeat (dec this.repeat))) ... repeatDelayTime ...)
      (let []
        (doseq [k (goog.object.getKeys end-val)]
          (when-not (undefined? (goog.object.get start-val k))
            (let [start (or (goog.object.get start-val k) 0)
                  end (goog.object.get end-val k)]
              (if (array? end)
                nil ; (goog.object.set this.object k (.interpolationFunction this end value))
                (goog.object.set this.v k end)))))
        false))))

(defn ^boolean tween-array-state
  [this]
  (let [transition (.-transition this)
        start-val  (.-start-val this)
        start-time (.-start-time this)
        end-val    (.-end-val this)
        curve      (get transition :curve)
        duration   (get transition :duration)
        time-since-start (- (now) start-time)]
    (if (< time-since-start duration)
      (let [t  (/ time-since-start duration)
            stop (alength end-val)]
        (loop [i 0]
          (when (< i stop)
            (let [start (aget start-val i)
                  end   (aget end-val i)]
              (aset (.-v this) i (goog.math.lerp start end (curve t)))
              (recur (inc i)))))
        true)
      ; (if (< 0 this.repeat)
      ;   (when (js/isFinite this.repeat)
      ;     (set! this.repeat (dec this.repeat))) ... repeatDelayTime ...)
      (let [stop (alength end-val)]
        (loop [i 0]
          (when (< i stop)
            (let [start (aget start-val i)
                   end   (aget end-val i)]
              (aset (.-v this) i end)
              (recur (inc i)))))
        false))))

(defn ^boolean valid-number? [n] (and (number? n) (js/isFinite n)))

(defn ^boolean valid-array? [a]
  (and (goog.isArrayLike a) (every? valid-number? (array-seq a))))

(deftype NumTweener [^:mutable v]
  IDeref
  (-deref [this] (.-v this))
  ITweener
  (set-value! [this v]
    (assert (valid-number? v))
    (do
      (set! (.-end-val this) v)
      (set! (.-v this) v)))
  (update-state ^boolean [this] (tween-num-state this))
  (tween! [this transition]
    (assert (valid-number? (get transition :from @this))  "origin value must be finite number")
    (assert (valid-number? (get transition :to)) "destination value must be finite number")
    (start-tweening! this transition))
  (tween! [this start end transition]
    (js/console.warn "use arity-2 (neo/tween! twnr {:from ... :to ...}) form instead of arity 4.")
    (start-tweening! this (assoc transition :from start :to end))))

(defn assert-valid-object-transition!
  "only care about k-v pairs that are actually tweened. a value object is free
   to have other entries that are just ignored."
  [tweener transition]
  (let [to (get transition :to)
        from (get transition :from)
        [from default?] (if from [from false] [@tweener true])
        to-keys (goog.object.getKeys to)
        from-keys (goog.object.getKeys from)
        shared-keys (clojure.set.intersection (set to-keys) (set from-keys))]
    (doseq [k shared-keys]
      (let [to-val (goog.object.get to k)
            from-val (goog.object.get to k)]
        (assert (not (or (goog.isArrayLike to-val) (goog.isArrayLike from-val)))
                "array interpolation not implemented yet")
        (assert (or (nil? from-val) (valid-number? from-val))
                (str "invalid origin value for key" (pr-str k)  " : " (pr-str from-val) ", default?" default? ))
        (assert (valid-number? to-val)
                (str "destination value for key" (pr-str k) " must be number, not"  (pr-str to-val)))))))

(deftype ObjectTweener [v]
  IDeref
  (-deref [this] v)
  ITweener
  (set-value! [this v]
    (assert (object? v))
    (do
      (set! (.-end-val this) v)
      (set! (.-v this) v)))
  (update-state ^boolean [this] (tween-object-state this))
  (tween! [this transition]
    (when ^boolean goog.DEBUG (assert-valid-object-transition! this transition))
    (start-tweening! this transition)))

(deftype ArrayTweener [v]
  IDeref
  (-deref [this] v)
  ITweener
  (set-value! [this v]
    (assert (valid-array? v))
    (do
      (set! (.-end-val this) v)
      (set! (.-v this) v)))
  (update-state ^boolean [this] (tween-array-state this))
  (tween! [this transition]
    (when ^boolean goog.DEBUG
      (let [from (get transition :from @this)
            to (get transition :to)]
        (assert (valid-array? from))
        (assert (and (valid-array? to) (<= (alength from) (alength to))))))
    (start-tweening! this transition)))

(defn Tweener
  "@param{(Object|IArrayLike|Number)} v
   @return{(NumTweener|ArrayTweener|ObjectTweener)}"
  [v]
  (if (number? v)
    (NumTweener. v)
    (if (goog.isArrayLike v)
      (ArrayTweener. v)
      (if (object? v)
        (ObjectTweener. v)
        (throw (js/Error. (str "unsupported tweener value type " (pr-str (type v)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-value! ;;;;;;;;;;;;;;;;;; TODO this should be wrapped to validate return values
  "change the tweener value with fn + ?args (..like swap!)"
  ([tweener f] (set-value! tweener (f @tweener)))
  ([tweener f a] (set-value! tweener (f @tweener a)))
  ([tweener f a b] (set-value! tweener (f @tweener a b))))

(defn cancel!
  "cancel animation. opt arg to set resting value"
  ([this] (cancel! this nil))
  ([this val]  ;;;;;;;;;;;;;;;;;; TODO this should be wrapped to validate return values
   (do
     (if val (set! (.-v this) val))
     (if (some? (.-onCancel this))
       (.onCancel this @this))
     (set! (.-active? this) false)
     (set! (.-dirty? this) false))))

(defn end-value
  "returns the destination value of in-progress animation"
  [tweener] (or (.-end-val tweener) @tweener))

(defn- dirty? [tweener] (.-dirty? tweener))

(defn- update-tweener! [tweener]
  (if ^boolean (.-active? tweener)
    (do
      (if-not ^boolean (update-state tweener)
        (set! (.-active? tweener) false))
      (if (some? (.-onUpdate tweener))
        (.onUpdate tweener @tweener)))
    (when ^boolean (.-dirty? tweener)
      (set! (.-dirty? tweener) false)
      (when-let [cb (get (.-transition tweener) :cb)]
        (js/console.warn "stop using :cb transition key, use :onFinish")
        (cb))
      (if (some? (.-onFinish tweener))
        (.onFinish tweener @tweener)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce raf-id (atom nil))
(defonce registry-add-queue #js[])
(defonce registry #js[])
(defonce every-ticks #js[])
(defonce next-ticks #js[])

(defn ^boolean running? [] (some? @raf-id))

(defn next-tick ;rename nextTick
  "Call a function once at the beginning of the next cycle,
   before every-tick and render"
  [f]
  (and (< (.indexOf next-ticks f) 0) (.push next-ticks f)))

(defn cancel-next-tick [f]
  (let [i (.indexOf next-ticks f)]
    (and (<= 0 i) (.splice next-ticks i 1))))

(defn every-tick
  "Call a function every cycle, after nextticks but before render"
  [f]
  (and (< (.indexOf every-ticks f) 0) (.push every-ticks f)))

(defn cancel-every-tick [f]
  (let [i (.indexOf every-ticks f)]
    (and (<= 0 i) (.splice every-ticks i 1))))

(defn clear-every-tick! [] (set! (.-length every-ticks) 0))

(defn- add-tweener-group
  "updates begin on next cycle"
  [tweener-group]
  (set! (.-disabled tweener-group) false)
  (and (< (.indexOf registry tweener-group) 0)
       (.push registry-add-queue tweener-group)))

(defn- remove-tweener-group
  "flags a tweener group to skip updates and be asynchronously removed next cycle"
  [tweener-group]
  (set! (.-disabled tweener-group) true)
  (next-tick
    #(let [i (.indexOf registry tweener-group)]
       (and (<= 0 i) (.splice registry i 1)))))

(defn clear-registry! [] (set! (.-length registry) 0))

(deftype TweenerGroup [owner tweener-array disabled])

(defn render-component [component] (.forceUpdate component))

(def *render-fn* render-component)

(defn- tick! []
  ;;;; next-ticks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let [stop (alength next-ticks)]
    (loop [i 0]
      (when  ^boolean (< i stop)
        ((aget next-ticks i))
        (recur (inc i)))))
  (set! (.-length next-ticks) 0)
  ;;;; every-ticks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let [stop (alength every-ticks)]
    (loop [i 0]
      (when  ^boolean (< i stop)
        ((aget every-ticks i))
        (recur (inc i)))))
  ;;; dirty-check-loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let [stop (alength registry)]
    (loop [i 0]
      (when (< i stop)
        (let [tweener-group (aget registry i)]
          (when-not ^boolean (.-disabled tweener-group)
            (let [tweener-array (.-tweener-array tweener-group)]
              (let [stop (alength tweener-array)]
                (loop [j 0]
                  (when (< j stop)
                    (update-tweener! (aget tweener-array j))
                    (recur (inc j)))))
              (if ^boolean (goog.array.some tweener-array dirty?)
                (*render-fn* (.-owner tweener-group))))))
        (recur (inc i)))))
  ;;;; merge in tweeners added during current cycle
  (when (< 0 (alength registry-add-queue))
    (.apply (.-push registry) registry registry-add-queue)
    (set! (.-length registry-add-queue) 0))
  (reset! raf-id (js/requestAnimationFrame tick!)))

(defn start! []
  (when-not (running?)
    (tick!)))

(defn stop! [] (reset! raf-id (js/cancelAnimationFrame @raf-id)))

(defonce owner-table (atom {})) ;owner->tweener-group

(defn clear-all! []
  (set! (.-length next-ticks) 0)
  (clear-every-tick!)
  (clear-registry!)
  (reset! owner-table {}))

(defn register-owner
  "Group tweeners by owner and include them in the update loop.
   If an owner is already registered, its existing entry will be overwritten.
   @param {!Object} owner :: what to render when associated tweeners flag dirty.
   @param {(!IMap|ISeq|Array)} xfs :: map of arbitrary keys to tweeners, or sequence of tweeners"
  [owner xfs]
  (when ^boolean goog.DEBUG
    (if (map? xfs)
      (assert (every? #(implements? ITweener %) (vals xfs)) "xfs maps must have all neo.core.Tweener vals.")
      (if (seq xfs)
        (assert (every? #(implements? ITweener %) xfs) "non-map xfs must consist entirely of tweeners")
        (throw (js/Error.  (str "xfs has unsupported shape: " (type xfs)))))))
  (let [tweener-array (into-array (if (map? xfs) (vals xfs) xfs))
        o (if-let [o (get @owner-table owner)]
            (do
              (set! (.-tweener-array o) tweener-array)
              o)
            (TweenerGroup. owner tweener-array false))]
    (swap! owner-table assoc owner o)
    (add-tweener-group o)))

; (set! (.-disabled tweener-group) true)

(defn deregister-owner
  "Remove an owner & its associated tweeners from the update-loop"
  [owner]
  (when-let [o (get @owner-table owner)]
    (remove-tweener-group o)
    (swap! owner-table dissoc owner)))
