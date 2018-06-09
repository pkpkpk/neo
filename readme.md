# ~~`[neo "0.1.0"]`~~
~~`neo {:mvn/version "0.1.0"}`~~


## A DOM Animation Library for Clojurescript
Inspired by *THREE.js* and the original *famous* , neo combines primitives for working with [css matrix3d transforms][1] with a simple api for managing animation state.

#### The Update Loop Cycle
+ Every Animation Frame:
  1. next-ticks (pre-render)
    - one-off callbacks registered with `neo.core/next-tick!` are called
    - the next-tick queue is emptied
  + every-ticks (pre-render)
    - callbacks registered with `neo.core/every-tick!` are called
    - these callbacks are called once every cycle until they are removed with `neo.core/cancel-every-tick!`
  + The Dirty-Check Cycle (rendering)
    - for every registered 'component', each of its associated tweeners is updated. If any tweener returns dirty, `neo.core/*render-fn*` is called on the owner, at most once per cycle.

#### Tweeners
The `neo.core/register-owner` fn takes a renderable object and a seq of associated Tweeners, and adds them to the update loop as a pair. Each time a Tweener flags dirty, `neo.core/*render-fn*` is called on the owner. By default neo assumes you want to `forceUpdate()` a react component, but you can set! the `*render-fn*` to be any fn(owner).

Tweeners come in 3 types:
  + NumTweener
  + ArrayTweener
  + ObjectTweener


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