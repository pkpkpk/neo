# ~~`[neo "0.1.0"]`~~
~~`neo {:mvn/version "0.1.0"}`~~


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

#### Tweeners
The `neo.core/register-owner` fn takes a renderable object and a seq of associated Tweeners, and adds them to the update loop as a pair. You want to do this *before* the component mounts so that tweeners are available as values to render impls. Owners are added on the *next-cycle* so as to not disturb the in-progress render tick.

Any registered component must call `neo.core/deregister-owner` during `willUnmount`! Failure to do this is a memory leak and will drag your runtime to crawl fairly quickly. If your browser is crashing during dev, it is probably this. If you have a large population of animating components which pop in and out of existence on the fly (...which is the whole appeal of using react) then you must get this right.

Each time a Tweener flags dirty, `neo.core/*render-fn*` is called on the owner. If multiple tweeners flag dirty, the owner is still re-rendered only once per frame By default neo assumes you want to `forceUpdate()` a react component, but you can set! the `*render-fn*` to be any fn(owner).

Tweeners come in 3 types:
  + NumTweener
  + ArrayTweener
  + ObjectTweener
  
You initiate an animation by calling `(neo.core.tween! <tweener>  <transistion-map>)`. The `:from` key is an optional override of the tweener's current state. The `:to` key is a destination value. Both `:from` and `:to` must agree with the tweener type! For example, if you have an `ArrayTweener`,  `:to` & `:from` must be arrays too.

You can chain arbitrary animations together by nesting calls in transition-map callback keys (see below). Resist the urge to do expensive work every frame or during in progress animations; it's better to coordinate the complete a set of animations via `:onFinish` hooks and then kickoff some io or whatever.

#### Initiating transitions

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


##### Composing Transforms

##### Things that kill performance
 + blocking javascript engine optimizations
   - get your arities right
 + garbage pauses
 + reflows


[1]:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/matrix3d
