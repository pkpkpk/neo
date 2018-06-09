(ns neo.build
  (:require [cljs.build.api :as api]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def source-dir "src/main")

(def test-dir "src/test")

(def test-config {:target :nodejs
                  :main 'neo.runner
                  :output-to "target/tests.js"
                  :output-dir "target/out"
                  :aot-cache true ;; this needs to be disabled for type-checking
                  :parallel-build true})

(def periodic-config
  {:id "periodic"
   :src-path "examples/periodic/src"
   :compiler {:main 'periodic.core
              :parallel-build true
              :aot-cache true
              :asset-path "js/compiled/out"
              :output-to  "examples/periodic/resources/public/js/compiled/periodic.js"
              :output-dir "examples/periodic/resources/public/js/compiled/out"}})

(defmulti task first)

(defmethod task  "periodic" [[_ _]]
  (let [config (:compiler periodic-config)]
    (println "building periodic to" (:output-to config))
    (api/build (:src-path periodic-config) config)))

(defn run-node-tests []
  (let [{:keys [out err exit]} (shell/sh "node" "target/tests.js" :env {:SOME_ENV_VAR "some-env-value"})]
    (println out err)
    (= exit 0)))

(defn test-once [config]
  (api/build (api/inputs source-dir test-dir) config)
  (let [success? (run-node-tests)]
    (System/exit (if success? 0 1))))

(defmethod task "test" [[_ type]]
  (case type
    (nil "once") (test-once test-config)
    (do (println "Unknown argument to test task:" type)
        (System/exit 1))))

(defmethod task :default
  [args]
  (let [all-tasks (-> task methods (dissoc :default) keys sort (->> (interpose ", ") (apply str)))]
    (println "unknown or missing task argument. Choose one of:" all-tasks)
    (System/exit 1)))

(task *command-line-args*)
