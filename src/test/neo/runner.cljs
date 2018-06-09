(ns neo.runner
  (:require [cljs.test :refer-macros [run-tests]]
            [cljs.nodejs :as nodejs]
            [neo.core-tests]
            [neo.object3D-tests]
            [neo.cam-tests]
            [neo.vec.affine-tests]
            [neo.vec.euler-tests]
            [neo.vec.quat-tests]
            [neo.vec.vec2-tests]
            [neo.vec.vec3-tests]
            [neo.vec.vec4-tests]
            [neo.vec.mat4-tests]))

(nodejs/enable-util-print!)

(defn -main []
  (when-not (exists? js/requestAnimationFrame)
    (try
      (.polyfill (js/require "raf"))
      (catch js/Error e
        (js/process.stderr.write "missing requestAnimationFrame polyfill, please `npm install` or `npm install raf`")
        (js/process.exit 1))))
  (run-tests
   'neo.core-tests
   'neo.object3D-tests
   'neo.cam-tests
   'neo.vec.affine-tests
   'neo.vec.euler-tests
   'neo.vec.quat-tests
   'neo.vec.vec2-tests
   'neo.vec.vec3-tests
   'neo.vec.vec4-tests
   'neo.vec.mat4-tests))

(set! *main-cli-fn* -main)