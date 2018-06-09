(defproject neo "0.0.1"

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/tools.reader "1.2.1"]]

  :plugins [[lein-cljsbuild "1.1.7"]]

  :clean-targets ^{:protect false} ["target" "examples/periodic/resources/public/js/compiled"]

  :source-paths ["src/main" "src/test"]

  :cljsbuild {
    :builds [{:id "tests"
              :source-paths ["src/main" "src/test"]
              :compiler {:target :nodejs
                         :main neo.runner
                         :output-to "target/neo.js"
                         :output-dir "target/out"
                         :parallel-build true
                         :compiler-stats true
                         :optimizations :simple}}]}

  :profiles
  {:dev {:dependencies [[org.omcljs/om "1.0.0-beta3"]]
         :cljsbuild {:builds
                     [{:id "periodic"
                       :source-paths ["src" "examples/periodic/src"]
                       :compiler {:main periodic.core
                                  :asset-path "js/compiled/out"
                                  :output-to  "examples/periodic/resources/public/js/compiled/periodic.js"
                                  :output-dir "examples/periodic/resources/public/js/compiled/out"}}]}}})