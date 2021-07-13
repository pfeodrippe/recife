(ns example.partner-2
  (:require
   [clojure.set :as set]
   [medley.core :as medley]
   [recife.anim :as ra]
   [recife.core :as r]))

(def global
  {::companies {:c1 {:children #{:c2}}
                :c2 {}
                :c3 {:children #{:c1}}
                :c4 {}}
   :partner/reqs #{}
   :webhook/reqs #{}
   :id/counter 0
   :partner/history []})

(r/defproc initial-request {}
  (fn [{:keys [::companies] :as db}]
    (let [companies-with-no-children (->> companies
                                          (medley/remove-vals (comp seq :children))
                                          ;; Add a flag indicating that the company was already sent so
                                          ;; we can get rid of the duplication violations.
                                          (medley/map-vals #(assoc % :sent? true)))]
      (-> db
          (update :partner/reqs set/union (->> companies-with-no-children keys set))
          (update ::companies merge companies-with-no-children)
          (r/done)))))

(r/defproc partner-server {}
  (fn [{:keys [:partner/reqs :id/counter ::companies] :as db}]
    ;; TODO: Check for unsent companies.
    ;; TODO: Maybe use non determinism instead of `first` to fetch
    ;; a partner request.
    (if (every? (comp :id val) companies)
      (r/done db)
      (when-let [company-name (first reqs)]
        (-> db
            (update :partner/reqs set/difference #{company-name})
            (update :webhook/reqs conj [company-name counter])
            (update :partner/history conj [company-name counter])
            (update :id/counter inc))))))

(r/defproc webhook {:procs #{:w1}
                    :local {:pc :webhook/handle-request}}
  {:webhook/handle-request
   (fn [{:keys [:webhook/reqs ::companies] :as db}]
     (if (every? (comp :id val) companies)
       (r/done db)
       (when-let [[company-name id :as req] (first reqs)]
         (let [{companies' ::companies :as new-db}
               (-> db
                   (update :webhook/reqs set/difference #{req})
                   (assoc-in [::companies company-name :id] id))]
           (-> new-db
               (assoc :loaded-companies companies')
               (r/goto :webhook/send-to-partner))))))

   :webhook/send-to-partner
   (fn [{:keys [:loaded-companies] :as db}]
     ;; TODO: Fetch one ready company (not many) non deterministically.
     ;; TODO: Do not send if it's in transit (`:sent?`).
     (let [companies-ready (->> loaded-companies
                                (medley/remove-vals :id)
                                ;; Remove sent companies.
                                (medley/remove-vals :sent?)
                                (medley/filter-vals (fn [{:keys [:children]}]
                                                      (every? #(-> loaded-companies % :id) children)))
                                keys
                                set)]
       (-> db
           (update :partner/reqs set/union companies-ready)
           (r/goto :webhook/handle-request))))})

(r/definvariant no-partner-history-duplicates
  (fn [{:keys [:partner/history]}]
    (= (->> history (map first) distinct count)
       (count history))))

;; All companies will have an `:id` in the end of everything.
;; We say that at some point in time (`:eventually`) and forever
;; (`always`) the companies will have an id (which is our main goal).
(r/defproperty all-companies-will-have-an-id
  [:eventually
   [:always
    (fn [{:keys [::companies]}]
      (->> (vals companies)
           (every? :id)))]])

(comment

  (def result
    (r/run-model global
                 #{initial-request partner-server
                   webhook no-partner-history-duplicates
                   all-companies-will-have-an-id}
                 {:gen-trace-example? true}))

  (ra/visualize-result result)

  ;; TODO:
  ;; - [ ] Add invariants.
  ;; - [ ] Add temporal properties.
  ;; - [ ] Add two partner and two webhook processes.

  ())
