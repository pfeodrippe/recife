(ns recife.analyzer
  "Functions to help you compare your implementation with the generated
  traces."
  (:require
   [arrudeia.core :as ar]
   [recife.core :as r]))

(defn analyze
  [result {:keys [:init :implementation-mapping :number-of-traces :debug?]
           :or {number-of-traces 20}}]
  (loop [[trace & other-traces] (-> result
                                    r/states-from-result
                                    (r/random-traces-from-states number-of-traces))
         trace-counter 0]
    (when debug? (println :trace-counter trace-counter))
    (let [implementation-step->model-step (->> implementation-mapping
                                               (mapv (juxt (comp :step val) key))
                                               (into {}))
          ;; Reset semaphore.
          _ (swap! ar/semaphore (constantly {:debug []}))
          initial-global-state (dissoc (get-in trace [0 1]) ::r/procs)
          steps (->> trace
                     (drop 1)
                     (mapv (comp :context :recife/metadata last))
                     (mapv (fn [[step context]]
                             (let [impl (implementation-mapping step)]
                               [(:self context) {:context context
                                                 :impl-step (:step impl)}]))))
          ;; TODO: Stop any remaining proc so it does not leak. Exceptions should
          ;; also kill them. Is this fixed as long we run all the steps?

          ;; Refactor this, it's being used only to find the cancelled indexes.
          proc-name->proc (->> steps
                               (map first)
                               distinct
                               (mapv (fn [name]
                                       ;; TODO: Find cancelled indexes using some other way.
                                       [name {:proc-name name}]))
                               (into {}))
          cancelled-indexes (ar/find-cancelled-indexes (ar/parse-process-names proc-name->proc steps))
          ;; Set initial state.
          _ (init initial-global-state)
          ;; Run the steps.
          {:keys [:error? :results]}
          (->> steps
               (reduce (fn [{:keys [:idx :proc-name->proc] :as acc} [self {:keys [:impl-step :context]}]]
                         (let [
                               ;; Fetch attributes from the root map for this step.
                               {:keys [:register :guard :reload? :handler :assertion]}
                               (-> impl-step
                                   implementation-step->model-step
                                   implementation-mapping)

                               params {:context context
                                       :model-state (last (get trace (inc idx)))
                                       :model-previous-state (last (get trace idx))}
                               ;; Create a proc or return a existent proc.
                               ;; `guard` is used for situations where you don't
                               ;; want to register unnecessarily.
                               proc (when (or (nil? guard)
                                              (guard params))
                                      (or (when register
                                            (ar/register self (register params)))
                                          (proc-name->proc self)))
                               ;; Run step using arrudeia, internally it's async
                               ;; and may hang our process if called for a step
                               ;; which does not really exist.
                               run-step-result
                               (when proc
                                 (ar/run-step [proc impl-step]
                                              {:step-opts
                                               (when (or reload?
                                                         (contains? cancelled-indexes idx))
                                                 {:cancel-execution? true})}))

                               output-map
                               {:output (if handler
                                          (handler (assoc params :step-result run-step-result))
                                          run-step-result)
                                :impl-step impl-step
                                :context context}

                               {:keys [:expected :actual] :as assertion-result}
                               (when assertion
                                 (assertion (assoc output-map
                                                   :model-state (last (get trace (inc idx)))
                                                   :model-previous-state (last (get trace idx)))))]
                           (if (when assertion
                                 (or (and expected actual
                                          (not= expected actual))
                                     (not assertion-result)))
                             (-> acc
                                 (update :results conj
                                         [(inc idx)
                                          [self (merge output-map
                                                       {:error {:code :assertion-failed
                                                                :result assertion-result}})]])
                                 (update :idx inc)
                                 (update :proc-name->proc assoc self proc)
                                 (assoc :error? true))
                             (-> acc
                                 (update :idx inc)
                                 (update :results conj [(inc idx) [self output-map]])
                                 (update :proc-name->proc assoc self proc)))))
                       {:idx 0
                        :results []
                        :proc-name->proc {}}))]
      (cond
        ;; If error, return the results until first error (inclusive).
        error? {:results (concat (take-while #(-> % last last :error nil?) results)
                                 [(first (filter #(-> % last last :error) results))])
                :trace trace}
        (seq other-traces) (recur other-traces (inc trace-counter))))))
