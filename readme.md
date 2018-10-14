# `[neo-cljs "0.1.0"]`
`neo-cljs {:mvn/version "0.1.0"}`

[![Clojars Project](https://img.shields.io/clojars/v/neo-cljs.svg)](https://clojars.org/neo-cljs)

## A DOM Animation Library for Clojurescript
Inspired by *THREE.js* and the original *famous* , neo combines primitives for working with [css matrix3d transforms][1] with a simple api for managing animation state.

#### The Update Loop Cycle
Every Animation Frame:
  1. next-ticks (pre-render)
      - one-off callbacks registered with `neo.core/next-tick!` are called
      - the next-tick queue is emptied
  2. every-ticks (pre-render)
      - callbacks registered with `neo.core/every-tick!` are called
      - these callbacks are called once every cycle until they are removed with `neo.core/cancel-every-tick!`
  3. The Dirty-Check Cycle (rendering)
      - for every registered 'component', each of its associated tweeners is updated. If any tweener returns dirty, `neo.core/*render-fn*` is called on the owner, at most once per cycle.

#### Tweeners & React
*Note: you do not have to use react but the lifecycle principles are the same*

The `neo.core/register-owner` fn takes a renderable object and a seq (...a map is ok) of associated Tweeners, and adds them to the update loop as a pair.  Owners are added on the *next-cycle* so as to not disturb the in-progress render tick.

Tweeners deref to their current value. If you wish to use tweeners as values in your render impls then you should register owners on  `GetInitialState` or `ComponentWillMount` , otherwise they will not be there during the initial render.



```clojure
(require '[neo.core :as neo]
          [neo.vec.vec3 :as vec3])

{
  ; a reagent class ...
  :get-initial-state
  (fn [this]
    {:opacity (neo/tweener 0) ;<= retrieve in render and deref
     :position (neo/tweener (vec3/vec3))})

}

```


Any registered component must call `neo.core/deregister-owner` during `ComponentWillUnmount`. Failure to do this leaks the component in the dirty checker and doing this repeatedly will drag your runtime to crawl fairly quickly. If your browser is crashing during dev, it is probably this. If you have a large population of animating components which pop in and out of existence on the fly (...which is the whole appeal of using react) then you must get this right.

Each time a Tweener flags dirty, `neo.core/*render-fn*` is called on the owner. If multiple tweeners flag dirty, the owner is still re-rendered only once per frame. By default neo assumes you want to `forceUpdate()` a react component, but you can set! the `*render-fn*` to be any `fn<owner>`.

Tweeners come in 3 types:
  + NumTweener
  + ArrayTweener
  + ObjectTweener

You initiate an animation by calling `(neo.core.tween! <tweener>  <transistion-map>)`. The `:from` key is an optional override of the tweener's current state. The `:to` key is a destination value. Both `:from` and `:to` must agree with the tweener type! For example, if you have an `ArrayTweener`,  `:to` & `:from` must be arrays too.

You can chain arbitrary animations together by nesting more `tween!` calls in transition-map callback entries (see below). Resist the urge to do expensive work every frame or during in progress animations. If you have multiple components animating, it is good practice to coordinate their finished state via  `:onFinish` hooks and then kickoff some io or whatever than it is to introduce computation while things are still moving.

#### Designing Transitions


+ Transition Maps

  ```clojure
  {
    :from start-value ; must agree with the Tweener!
    :to end-value ; must agree with the Tweener!
    :curve (fn [t] ...) ; keyword or fn
    :duration 3000 ;milliseconds
    :delay 100 ;milliseconds
    :onStart (fn [v] ...)
    :onUpdate (fn [v] ...)
    :onFinish  (fn [v] ...)
    :onCancel (fn [v] ...)
  }
  ```

##### Affine Transforms

##### Perspective, Cameras, Object3D

##### Things that kill performance
 + blocking javascript engine optimizations
   - get your arities right
 + garbage pauses
 + reflows
 + bad render functions


[1]:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/matrix3d
