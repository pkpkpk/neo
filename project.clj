(defproject neo-cljs "0.1.0"
  :description "A DOM animation lib for clojurescript"
  :url "https://github.com/pkpkpk/neo"
  :repositories [["clojars" {:sign-releases false}]]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 [org.clojure/core.async "0.4.474"]]

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
                                  :optimizations :advanced
                                  :output-to  "examples/periodic/resources/public/js/compiled/periodic.js"
                                  :output-dir "examples/periodic/resources/public/js/compiled/out"}}]}}})