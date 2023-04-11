(ns recife.webdriver
  "Functions to help you webdrive a browser using Recife."
  (:require
   [clojure.test :refer [testing report]]
   [medley.core :as m]
   [recife.core :as r]))

(def ^:private original-report
  report)

(defn handle-states
  [history state states-mapping]
  (let [previous-state (last (last history))]
    (->> states-mapping
         (mapv (fn [[k {:keys [check snapshot]}]]
                 [k (if (contains? state k)
                      (let [*test-info (atom [])]
                        (with-redefs [report (fn [event]
                                               (swap! *test-info conj
                                                      {:event event
                                                       :testing-contexts
                                                       (vec (reverse clojure.test/*testing-contexts*))})
                                               (original-report event))]
                          (testing k
                            (let [impl-state (snapshot state)
                                  result (check previous-state (assoc-in state [:-impl k] impl-state))]
                              (if result
                                {:result result
                                 :impl-state impl-state}
                                {:result result
                                 :impl-state impl-state
                                 :test-info @*test-info})))))
                      {:result true})]))
         (into {}))))

(defn drive-trace
  [trace {:keys [init procs-mapping states-mapping]}]
  (let [initial-global-state (dissoc (get-in trace [0 1]) ::r/procs)
        _ (init initial-global-state)]
    (loop [[[idx {:keys [recife/metadata] :as state}] & next-states] trace
           history []]
      (if (nil? idx)
        {:trace history}
        (let [check (fn check
                      [violation step]
                      (let [states (handle-states history state states-mapping)
                            errors (->> states
                                        (remove (comp :result second))
                                        (into {}))]
                        (if (seq errors)
                          {:type violation
                           :step step
                           :errors errors
                           :trace history}
                          {:history (conj history [idx (assoc state :-impl
                                                              (update-vals states :impl-state))])})))]
          (if metadata
            (let [{:keys [context]} metadata
                  step (first context)
                  step-fn (procs-mapping step)]
              (when-not step-fn
                (throw (ex-info "Handler not implemented for step" {:step step})))
              (step-fn context)
              (let [res (check :violation step)]
                (if (:errors res)
                  res
                  (recur next-states (:history res)))))

            ;; Initial state.
            (let [res (check :initial-state-violation nil)]
              (if (:errors res)
                res
                (recur next-states (:history res))))))))))

#_{:clj-kondo/ignore [:unused-binding]}
(defn drive
  "Drive a browser using Recife's result. It returns a map with `:errors` or
  the traces if successful.

  `max-number-of-states` is per trace."
  [result {:keys [init procs states max-number-of-traces max-number-of-states]
           :or {max-number-of-traces 10
                max-number-of-states 10}
           :as params}]
  (let [params (update params
                       :states-mapping
                       ;; Adapt states mapping.
                       (fn [states-mapping]
                         (->> states-mapping
                              (m/map-vals (fn [{:keys [check snapshot]
                                                :or {check (constantly true)
                                                     snapshot (constantly nil)}
                                                :as v}]
                                            (if (fn? v)
                                              {:check v
                                               :snapshot snapshot}
                                              {:check check
                                               :snapshot snapshot}))))))]
    (loop [[trace & other-traces] (-> result
                                      r/states-from-result
                                      (r/random-traces-from-states
                                       {:max-number-of-traces max-number-of-traces
                                        :max-number-of-states max-number-of-states}))
           trace-counter 0
           info []]
      (if trace
        (let [result (drive-trace trace params)]
          (if (:errors result)
            result
            (recur other-traces (inc trace-counter) (conj info result))))
        {:info info}))))
