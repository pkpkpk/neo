(ns neo.core-tests
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.test :refer-macros [deftest is testing async are use-fixtures]]
            [cljs.core.async :refer [<! >! close! put! take! chan alts! timeout] :as async]
            [neo.core :as neo]))

(defn log [& args]
  (.apply js/console.log js/console (into-array args)))

(defn set-render-fn [f] (set! neo/*render-fn* f))

(deftest tick-semantics-test
  (let [c (chan)
        everytick #(put! c :everytick)
        nexttick #(put! c :nexttick)]
    (async done
      (go
       (testing "next tick occurs before everytick"
         (neo/every-tick everytick)
         (neo/next-tick nexttick)
         (is (= :nexttick (<! c)))
         (is (= :everytick (<! c))))
       (testing "nexttick is cleared once called"
         (is (= :everytick (<! c))))
       (testing "tick calls from the same function do not stack"
         (dotimes [_ 10]
           (neo/next-tick nexttick))
         (is (= :nexttick (<! c)))
         (is (= :everytick (<! c))))
       (testing "tick calls from distinct functions do stack, in order added"
         (do
           (neo/next-tick nexttick)
           (neo/next-tick #(put! c :nexttick-2))
           (neo/next-tick #(put! c :nexttick-3)))
         (is (= :nexttick (<! c)))
         (is (= :nexttick-2 (<! c)))
         (is (= :nexttick-3 (<! c)))
         (is (= :everytick (<! c))))
       (testing "cancel everytick"
         (neo/cancel-every-tick everytick)
         (let [to (timeout 60)
               [v ch] (alts! [c to])]
           (is (= ch to) "once everytick is canceled should be no puts to c")))
       (done)))))

(deftest render-test
  (let [c (chan)
        render-fn (fn [owner] (put! c (get owner :id)))
        _ (set-render-fn render-fn)
        everytick #(put! c :everytick)
        nexttick #(put! c :nexttick)
        ownerA {:id :a}
        xa (neo/Tweener 0)
        ya (neo/Tweener 0)
        ownerB {:id :b}
        xb (neo/Tweener 0)
        expired (chan)]
    (async done
      (go
       (neo/every-tick everytick)
       (neo/next-tick nexttick)
       (neo/register-owner ownerA {:xa xa :ya ya})
       (neo/register-owner ownerB {:xb xb})
       (neo/tween! xa  {:duration 1
                        :from 0 :to 100
                        :onFinish #(close! expired)})
       (neo/tween! xb {:duration 1 :from 0 :to 100})
       (neo/tween! ya {:duration 1 :from 0 :to 100})
       (is (= :nexttick (<! c)) "nexttick occurs at start of a cycle")
       (is (= :everytick (<! c)) "everytick is before render")
       (is (= :everytick (<! c)) "registration mid-cycle means you must wait til next tick to begin")
       (is (= :a (<! c)) "dirty tweener should cause owner to render. two dirty tweeners but only 1 render per cycle")
       (is (= :b (<! c)) "dirty tweener should cause owner to render")
       (is (= :everytick (<! c)) "new cycle")
       (neo/cancel-every-tick everytick)
       (is (nil? (<! expired)) "transition map :cb is called after animation expires")
       (is (not (neo/dirty? xa)) "tweener should toggle back to clean after animation expires")
       (loop [chs [c (timeout 50)]]
         (let [[v ch] (alts! chs)]
           (if v
             (recur [c (timeout 50)]))))
       (testing "transition :delay"
         (neo/tween! xa {:duration 1 :delay 75 :from 0 :to 100})
         (let [[v ch] (alts! [(timeout 50) c])]
           (is (nil? v))
           (is (= :a (<! c)) "render occurs after specified delay")))
       (done)))))

(defn setup []
  (neo/clear-all!)
  (neo/start!))

(defn teardown []
  (neo/stop!)
  (neo/clear-all!)
  (set-render-fn neo/render-component))

(use-fixtures :each {:before setup :after teardown})