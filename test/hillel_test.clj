(ns hillel-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [clojure.walk :as walk]
   [hillel.ch1-wire-1 :as ch1-wire-1]
   [hillel.ch1-wire-2 :as ch1-wire-2]
   [hillel.ch2-recycler-1 :as ch2-recycler-1]
   [hillel.ch2-recycler-2 :as ch2-recycler-2]
   [hillel.ch5-cache-3 :as ch5-cache-3]
   [hillel.ch5-server-3 :as ch5-server-3]
   [hillel.ch6-threads-1 :as ch6-threads-1]
   [hillel.ch6-threads-2 :as ch6-threads-2]
   [recife.core :as r]
   matcher-combinators.test)
  (:import
   (lambdaisland.deep_diff2.diff_impl Mismatch Deletion Insertion)))

(def ^:private default-options
  {:seed 1
   :fp 0
   :workers 1})

(defn- simulate-assert
  [result]
  (when (= (get-in result [:trace-info :type]) :back-to-state)
    (testing "back to state violation - check loopback"
      (is (= (r/simulate (r/context-from-state (nth (:trace result)
                                                    (get-in result [:trace-info :violation :state-number])))
                         (last (:trace result)))
             (second (nth (:trace result)
                          (get-in result [:trace-info :violation :state-number])))))))

  (testing "simulate"
    (doseq [i (range (dec (count (:trace result))))]
      (testing i
        (is (= (r/simulate (r/context-from-state (nth (:trace result) (inc i)))
                           (nth (:trace result) i))
               (second (nth (:trace result) (inc i)))))))))

(deftest ch1-wire-1-test
  (let [result (r/run-model ch1-wire-1/global
                            #{ch1-wire-1/wire
                              ch1-wire-1/invariant}
                            default-options)]
    (is (= {:distinct-states 319
            :generated-states 417
            :fp 0
            :trace [[0
                      {:account/alice 5
                       :account/bob 5
                       :recife.core/procs {:x {:amount 4 :pc :hillel.ch1-wire-1/check-funds}
                                           :y {:amount 2 :pc :hillel.ch1-wire-1/check-funds}}}]
                     [1
                      {:account/alice 5
                       :account/bob 5
                       :recife/metadata {:context [:hillel.ch1-wire-1/check-funds {:self :y}]}
                       :recife.core/procs {:x {:amount 4 :pc :hillel.ch1-wire-1/check-funds}
                                           :y {:amount 2 :pc :hillel.ch1-wire-1/withdraw}}}]
                     [2
                      {:account/alice 5
                       :account/bob 5
                       :recife/metadata {:context [:hillel.ch1-wire-1/check-funds {:self :x}]}
                       :recife.core/procs {:x {:amount 4 :pc :hillel.ch1-wire-1/withdraw}
                                           :y {:amount 2 :pc :hillel.ch1-wire-1/withdraw}}}]
                     [3
                      {:account/alice 3
                       :account/bob 5
                       :recife/metadata {:context [:hillel.ch1-wire-1/withdraw {:self :y}]}
                       :recife.core/procs {:x {:amount 4 :pc :hillel.ch1-wire-1/withdraw}
                                           :y {:amount 2 :pc :hillel.ch1-wire-1/deposit}}}]
                     [4
                      {:account/alice -1
                       :account/bob 5
                       :recife/metadata {:context [:hillel.ch1-wire-1/withdraw {:self :x}]}
                       :recife.core/procs {:x {:amount 4 :pc :hillel.ch1-wire-1/deposit}
                                           :y {:amount 2 :pc :hillel.ch1-wire-1/deposit}}}]]
            :trace-info {:violation {:name :hillel.ch1-wire-1/invariant :type :invariant}}
            :seed 1}
           result))
    (simulate-assert result)))

(deftest ch1-wire-2-test
  (let [result (r/run-model ch1-wire-2/global
                            #{ch1-wire-2/wire
                              ch1-wire-2/invariant
                              ch1-wire-2/eventually-consistent}
                            (merge default-options
                                   {:tlc-args ["-lncheck" "final"]})) ]
    (is (= {:trace
            [[0
              {:recife.core/procs
               {:y {:amount 0 :pc :hillel.ch1-wire-2/check-and-withdraw}
                :x {:amount 1 :pc :hillel.ch1-wire-2/check-and-withdraw}}
               :account/bob 5
               :account/alice 5}]
             [1
              {:recife.core/procs
               {:y {:amount 0 :pc :hillel.ch1-wire-2/check-and-withdraw}
                :x {:amount 1 :pc :hillel.ch1-wire-2/deposit}}
               :account/bob 5
               :account/alice 4
               :recife/metadata
               {:context [:hillel.ch1-wire-2/check-and-withdraw {:self :x}]}}]
             [2
              {:recife.core/procs
               {:y {:amount 0 :pc :hillel.ch1-wire-2/deposit}
                :x {:amount 1 :pc :hillel.ch1-wire-2/deposit}}
               :account/bob 5
               :account/alice 4
               :recife/metadata
               {:context [:hillel.ch1-wire-2/check-and-withdraw {:self :y}]}}]
             [3
              {:recife.core/procs
               {:y {:amount 0 :pc :recife.core/done}
                :x {:amount 1 :pc :hillel.ch1-wire-2/deposit}}
               :account/bob 5
               :account/alice 4
               :recife/metadata
               {:context [:hillel.ch1-wire-2/deposit {:self :y}]}}]]
            :trace-info {:violation {:type :stuttering :state-number 4}}
            :distinct-states 223
            :generated-states 338
            :seed 1
            :fp 0}
           result))
    (simulate-assert result)))

(deftest ch2-recycler-1-test
  (let [result (r/run-model ch2-recycler-1/global
                            #{ch2-recycler-1/main ch2-recycler-1/invariant}
                            default-options)]
    (testing "whole result"
      (is (= {:trace :ok
              :trace-info nil
              :distinct-states 6
              :generated-states 7
              :seed 1
              :fp 0}
             result)))))

(deftest ch2-recycler-2-test
  (is (= {:trace :ok
          :trace-info nil
          :distinct-states 39218
          :generated-states 53618
          :seed 1
          :fp 0}
         (r/run-model ch2-recycler-2/global
                      #{ch2-recycler-2/main ch2-recycler-2/invariant}
                      default-options))))

(deftest ch5-cache-3-test
  (let [result (r/run-model ch5-cache-3/global
                            #{ch5-cache-3/cache
                              ch5-cache-3/time'
                              ch5-cache-3/invariant
                              ch5-cache-3/type-ok}
                            default-options)]
    (testing "trace result"
      (is (= {:trace
              [[0
                {:recife.core/procs
                 {:a2
                  {:resources-needed 1
                   :ran? false
                   :pc :hillel.ch5-cache-3/wait-for-resources}
                  :a1
                  {:resources-needed 1
                   :ran? false
                   :pc :hillel.ch5-cache-3/wait-for-resources}
                  :time {:pc :hillel.ch5-cache-3/tick}}
                 :cache/resource-cap 1
                 :cache/resources-left 1}]
               [1
                {:recife.core/procs
                 {:a2
                  {:resources-needed 1
                   :ran? false
                   :pc :hillel.ch5-cache-3/use-resources}
                  :a1
                  {:resources-needed 1
                   :ran? false
                   :pc :hillel.ch5-cache-3/wait-for-resources}
                  :time {:pc :hillel.ch5-cache-3/tick}}
                 :cache/resource-cap 1
                 :cache/resources-left 1
                 :recife/metadata
                 {:context [:hillel.ch5-cache-3/wait-for-resources {:self :a2}]}}]
               [2
                {:recife.core/procs
                 {:a2
                  {:resources-needed 1
                   :ran? false
                   :pc :hillel.ch5-cache-3/use-resources}
                  :a1
                  {:resources-needed 1
                   :ran? false
                   :pc :hillel.ch5-cache-3/use-resources}
                  :time {:pc :hillel.ch5-cache-3/tick}}
                 :cache/resource-cap 1
                 :cache/resources-left 1
                 :recife/metadata
                 {:context [:hillel.ch5-cache-3/wait-for-resources {:self :a1}]}}]
               [3
                {:recife.core/procs
                 {:a2
                  {:resources-needed 0
                   :ran? false
                   :pc :hillel.ch5-cache-3/use-resources}
                  :a1
                  {:resources-needed 1
                   :ran? false
                   :pc :hillel.ch5-cache-3/use-resources}
                  :time {:pc :hillel.ch5-cache-3/tick}}
                 :cache/resource-cap 1
                 :cache/resources-left 0
                 :recife/metadata
                 {:context
                  [:hillel.ch5-cache-3/use-resources {:x 0 :self :a2}]}}]
               [4
                {:recife.core/procs
                 {:a2
                  {:resources-needed 0
                   :ran? false
                   :pc :hillel.ch5-cache-3/use-resources}
                  :a1
                  {:resources-needed 0
                   :ran? false
                   :pc :hillel.ch5-cache-3/use-resources}
                  :time {:pc :hillel.ch5-cache-3/tick}}
                 :cache/resource-cap 1
                 :cache/resources-left -1
                 :recife/metadata
                 {:context
                  [:hillel.ch5-cache-3/use-resources {:x 0 :self :a1}]}}]]
              :trace-info
              {:violation
               {:type :invariant
                :name :hillel.ch5-cache-3/invariant
                :data {:well [:this :is :it]}}}
              :distinct-states 77
              :generated-states 93
              :seed 1
              :fp 0}
             result))
      (simulate-assert result))

    (testing "timeline diff"
      (is (= {:trace
              [[0
                {:recife.core/procs
                 {:a2
                  {:ran? false,
                   :pc :hillel.ch5-cache-3/wait-for-resources,
                   :resources-needed 1},
                  :a1
                  {:ran? false,
                   :pc :hillel.ch5-cache-3/wait-for-resources,
                   :resources-needed 1},
                  :time {:pc :hillel.ch5-cache-3/tick}},
                 :cache/resources-left 1,
                 :cache/resource-cap 1}]
               [1
                {:recife.core/procs
                 {:a2
                  {:ran? false,
                   :pc
                   {:- :hillel.ch5-cache-3/wait-for-resources,
                    :+ :hillel.ch5-cache-3/use-resources},
                   :resources-needed 1},
                  :a1
                  {:ran? false,
                   :pc :hillel.ch5-cache-3/wait-for-resources,
                   :resources-needed 1},
                  :time {:pc :hillel.ch5-cache-3/tick}},
                 {:+ :recife/metadata}
                 {:context [:hillel.ch5-cache-3/wait-for-resources {:self :a2}]}}]
               [2
                {:recife.core/procs
                 {:a2
                  {:ran? false,
                   :pc :hillel.ch5-cache-3/use-resources,
                   :resources-needed 1},
                  :a1
                  {:ran? false,
                   :pc
                   {:- :hillel.ch5-cache-3/wait-for-resources,
                    :+ :hillel.ch5-cache-3/use-resources},
                   :resources-needed 1},
                  :time {:pc :hillel.ch5-cache-3/tick}},
                 :recife/metadata
                 {:context
                  [:hillel.ch5-cache-3/wait-for-resources
                   {:self {:- :a2, :+ :a1}}]}}]
               [3
                {:recife.core/procs
                 {:a2
                  {:ran? false,
                   :pc :hillel.ch5-cache-3/use-resources,
                   :resources-needed {:- 1, :+ 0}},
                  :a1
                  {:ran? false,
                   :pc :hillel.ch5-cache-3/use-resources,
                   :resources-needed 1},
                  :time {:pc :hillel.ch5-cache-3/tick}},
                 :cache/resources-left {:- 1, :+ 0},
                 :recife/metadata
                 {:context
                  [{:- :hillel.ch5-cache-3/wait-for-resources,
                    :+ :hillel.ch5-cache-3/use-resources}
                   {:self {:- :a1, :+ :a2}, {:+ :x} 0}]}}]
               [4
                {:recife.core/procs
                 {:a2
                  {:ran? false,
                   :pc :hillel.ch5-cache-3/use-resources,
                   :resources-needed 0},
                  :a1
                  {:ran? false,
                   :pc :hillel.ch5-cache-3/use-resources,
                   :resources-needed {:- 1, :+ 0}},
                  :time {:pc :hillel.ch5-cache-3/tick}},
                 :cache/resources-left {:- 0, :+ -1},
                 :recife/metadata
                 {:context
                  [:hillel.ch5-cache-3/use-resources
                   {:x 0, :self {:- :a2, :+ :a1}}]}}]],
              :trace-info
              {:violation
               {:type :invariant,
                :name :hillel.ch5-cache-3/invariant,
                :data {:well [:this :is :it]}}},
              :distinct-states 77,
              :generated-states 93,
              :seed 1,
              :fp 0}
             (->> (r/timeline-diff result)
                  (walk/prewalk (fn [form]
                                  (cond
                                    (instance? Mismatch form) {:+ (:+ form) :- (:- form)}
                                    (instance? Deletion form) {:- (:- form)}
                                    (instance? Insertion form) {:+ (:+ form)}
                                    :else form)))))))))

(deftest ch5-server-3-test
  (let [result (r/run-model ch5-server-3/global
                            #{ch5-server-3/writer
                              ch5-server-3/reader
                              ch5-server-3/bounded-queue}
                            default-options)]
    (is (= {:trace
            [[0
              {:recife.core/procs
               {:writer {:pc :hillel.ch5-server-3/write}
                :reader {:pc :hillel.ch5-server-3/read}}
               :server/queue []}]
             [1
              {:recife.core/procs
               {:writer {:pc :hillel.ch5-server-3/write}
                :reader {:pc :hillel.ch5-server-3/read}}
               :server/queue [:msg]
               :recife/metadata
               {:context [:hillel.ch5-server-3/write {:self :writer}]}}]
             [2
              {:recife.core/procs
               {:writer {:pc :hillel.ch5-server-3/write}
                :reader {:pc :hillel.ch5-server-3/notify-failure
                         :current-message :msg}}
               :server/queue []
               :recife/metadata
               {:context [:hillel.ch5-server-3/read {:self :reader :notify-failure? true}]}}]
             [3
              {:recife.core/procs
               {:writer {:pc :hillel.ch5-server-3/write}
                :reader
                {:pc :hillel.ch5-server-3/notify-failure
                 :current-message :msg}}
               :server/queue [:msg]
               :recife/metadata
               {:context
                [:hillel.ch5-server-3/write {:self :writer}]}}]
             [4
              {:recife.core/procs
               {:writer {:pc :hillel.ch5-server-3/write}
                :reader
                {:pc :hillel.ch5-server-3/notify-failure
                 :current-message :msg}}
               :server/queue [:msg :msg]
               :recife/metadata
               {:context [:hillel.ch5-server-3/write {:self :writer}]}}]]
            :trace-info {:violation {:type :deadlock}}
            :distinct-states 18
            :generated-states 27
            :seed 1
            :fp 0}
           result))
    (simulate-assert result)))

(deftest ch6-threads-1-test
  (is (= {:trace
          [[0
            #:recife.core{:procs
                          {:t2 {:flag false :pc :hillel.ch6-threads-1/p1}
                           :t1 {:flag false :pc :hillel.ch6-threads-1/p1}}}]
           [1
            {:recife.core/procs
             {:t2 {:flag true :pc :hillel.ch6-threads-1/p2}
              :t1 {:flag false :pc :hillel.ch6-threads-1/p1}}
             :recife/metadata
             {:context [:hillel.ch6-threads-1/p1 {:self :t2}]}}]
           [2
            {:recife.core/procs
             {:t2 {:flag true :pc :hillel.ch6-threads-1/p2}
              :t1 {:flag true :pc :hillel.ch6-threads-1/p2}}
             :recife/metadata
             {:context [:hillel.ch6-threads-1/p1 {:self :t1}]}}]]
          :trace-info {:violation {:type :deadlock}}
          :distinct-states 8
          :generated-states 9
          :seed 1
          :fp 0}
         (r/run-model {} #{ch6-threads-1/thread ch6-threads-1/at-most-one-critical} default-options))))

(deftest ch6-threads-2-test
  (let [result (r/run-model {}
                            #{ch6-threads-2/thread
                              ch6-threads-2/at-most-one-critical
                              ch6-threads-2/no-livelocks}
                            (merge default-options
                                   {:tlc-args ["-lncheck" "final"]}))]
    ;; There is some difference running the test from cider in comparison with
    ;; running it from the CLI. So let's just assert that the violation is back
    ;; to state and the number of states, which we know are always the same.
    (is (match? {:trace-info {:violation {:type :back-to-state}}
                 :distinct-states 45
                 :generated-states 91
                 :seed 1
                 :fp 0}
                result))
    (simulate-assert result)))
