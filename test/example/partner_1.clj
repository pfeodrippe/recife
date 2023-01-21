(ns example.partner-1
  "This is a simple protocol for a integration which requires you to sign up
  children before you sign up any parent entities, it's required you to know the
  ids of the children beforehand (yes, it's a bad API design by our integration
  partner, but which we have to deal with nonetheless). It's a simplification of
  a real world spec which I wrote and found a violation, which made us change
  the implementation.

  Our entities are companies which can have children which also are companies,
  so you can have grandchildren or great-grandchildren (but no loops, you cannot
  have a child C1 who has a parent P1 where C1 has P1 as one of its children
  (transitively or directly)).

  See other `example.partner-*` namespaces for more checks and fixes.
  "
  (:require
   [clojure.set :as set]
   [recife.anim :as ra]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  ;; All global keywords should be namespaced so we can differentiate it
  ;; from local variables in our processes.
  {
   ;; `:c1`, `:c2` etc are the companies names (unique).
   ;; `:children` are the children of that company.
   ::companies {:c1 {:children #{:c2}}
                :c2 {}
                :c3 {:children #{:c1}}
                :c4 {}}

   ;; In the real world, we talk to our partner through a HTTP request, but here
   ;; we don't need to bother about implementation details like status, HTTP library
   ;; or error handling. We will model a API request by putting a element in a set,
   ;; which the partner process will consume.
   ;; Requests sent to the partner.
   :partner/reqs #{}

   ;; Requests sent to the webhook.
   :webhook/reqs #{}

   ;; Counter used by parter to give a id to an company.
   :id/counter 0

   ;; `:partner/history` stores the companies sent by the partner to the
   ;; webhook.
   :partner/history []})

(r/defproc initial-request
  (fn [{:keys [::companies] :as db}]
    ;; Initially, send only the companies with no children as there is no
    ;; `:id` that we can reference them in their parents.
    (let [companies-with-no-children (->> companies
                                          (remove (comp seq :children val))
                                          keys
                                          set)]
      (-> db
          (update :partner/reqs set/union companies-with-no-children)
          ;; We want this step to happen only in the beginning, so we will "close" it
          ;; with `r/done`.
          (r/done)))))

;; Represents the partner server, it just adds an id to the company and send it
;; to the webhook, which will handle it.
(r/defproc partner-server
  (fn [{:keys [:partner/reqs :id/counter ::companies] :as db}]
    ;; If all companies have an `:id` already, we are finished.
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
     ;; If all companies have an `:id` already, we are finished.
     (if (every? (comp :id val) companies)
       (r/done db)
       ;; With the usage of `when-let` here, the value could be `nil`,
       ;; which means that no state will change.
       (when-let [[company-name id :as req] (first reqs)]
         (let [{companies' ::companies :as new-db}
               (-> db
                   (update :webhook/reqs set/difference #{req})
                   (assoc-in [::companies company-name :id] id))]
           (-> new-db
               ;; Note that `:loaded-companies` is a local variable (unamespaced
               ;; keyword), so it will be available for the process which created
               ;; it (some of the keywords in `:procs`).
               ;; We are simulating here a load from a real database (using
               ;; possible stale data) which will be used in other steps of
               ;; this process.
               (assoc :loaded-companies companies')
               (r/goto :webhook/send-to-partner))))))

   :webhook/send-to-partner
   (fn [{:keys [:loaded-companies] :as db}]
     ;; Companies are ready to be sent when all of its children have
     ;; an `:id`.
     (let [companies-ready (->> loaded-companies
                                ;; If it has an `:id`, it was already sent,
                                ;; so we ignore them.
                                (remove (comp :id val))
                                (filter (fn [[_ {:keys [:children]}]]
                                          (every? #(-> loaded-companies % :id) children)))
                                keys
                                set)]
       (-> db
           (update :partner/reqs set/union companies-ready)
           (r/goto :webhook/handle-request))))})

;; We don't want the same company being sent twice to the partner server.
(rh/definvariant no-partner-history-duplicates
  [{:keys [:partner/history]}]
  (= (->> history (map first) distinct count)
     (count history)))

(comment

  ;; You can ask to generate a trace example (if a violation occurs, we will
  ;; return the violation as we normally do, the trace eaxample only happens
  ;; if every check was ok).
  (def result
    ;; No duplicates violation.
    @(r/run-model global
                  #{initial-request partner-server
                    webhook no-partner-history-duplicates}
                  {:trace-example? true}))

  (ra/visualize-result result)
  (-> result
      r/print-timeline-diff)

  ;; TODO for implementation:
  ;; - [x] Start implementation.
  ;; - [x] Add database (Postgres).
  ;; - [ ] Daycare center communication is through an HTTP request.

  ())
