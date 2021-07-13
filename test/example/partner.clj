(ns example.partner
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
  "
  (:require
   [clojure.set :as set]
   [recife.core :as r]))

(def global
  ;; All global keywords should be namespaced so we can differentiate it
  ;; from local variables in our processes.
  {
   ;; `:c1` and `:c2` are the companies names (unique).
   ;; `:children` are the children of that company.
   ::companies {:c1 {:children #{:c2}}
                :c2 {}}

   ;; In the real world, we talk to our partner through a HTTP request, but here
   ;; we don't need to bother about implementation details like status, HTTP library
   ;; or error handling. We will model a API request by putting a element in a set,
   ;; which the partner process will consume.
   ;; Requests sent to the partner.
   ::partner-reqs #{}

   ;; Requests sent to the webhook.
   ::webhook-reqs #{}

   ;; Counter used by parter to give a id to an company.
   ::id-counter 0})

(r/defproc initial-request {}
  (fn [{:keys [::companies] :as db}]
    ;; Initially, send only the companies with no children as there is no
    ;; `:id` that we can reference them in their parents.
    (let [companies-with-no-children (->> companies
                                          (remove (comp seq :children val))
                                          keys
                                          set)]
      (-> db
          (update ::partner-reqs set/union companies-with-no-children)
          ;; We want this step to happen only in the beginning, so we will "close" it
          ;; with `r/done`.
          (r/done)))))

;; Represents the partner server, it just adds a id to the account and send it
;; to the webhook, which will handle it.
(r/defproc partner-server {}
  (fn [{:keys [::partner-reqs ::id-counter] :as db}]
    ;; TODO: Check for unsent companies.
    ;; TODO: Maybe use non determinist instead of `first` to fetch
    ;; a partner request.
    (when-let [company-name (first partner-reqs)]
      (-> db
          (update ::partner-reqs set/difference #{company-name})
          (update ::webhook-reqs conj [company-name id-counter])
          (update ::id-counter inc)))))

(r/defproc webhook {:procs #{:w1}
                    :local {:pc ::webhook-init}}
  {::webhook-init
   (fn [{:keys [::webhook-reqs ::companies] :as db}]
     (when-let [[company-name id' :as req] (first webhook-reqs)]
       ;; If we already have a id, we don't need to use the one
       ;; from the request.
       (let [id (or (get-in companies [company-name :id])
                    id')]
         (-> db
             (update ::webhook-reqs set/difference #{req})
             (assoc-in [::companies company-name :id] id)
             ;; Note that `:pulled-accounts` is a local variable (unamespaced
             ;; keyword), so it will be available for the process which created
             ;; it (`:w1` or `:w2`).
             ;; We are simulating here a pull from a real database (using
             ;; possible stale data) which will be used in other steps of
             ;; this process.
             (assoc :pulled-accounts companies)))))})

(comment

  ;; You can ask to generate a trace example (if a violation occurs, we will
  ;; return the violation as we normally do, the trace eaxample only happens
  ;; if every check was ok).
  (-> (r/run-model global #{initial-request partner-server webhook} {:gen-trace-example? true})
      r/states-from-result)

  ;; TODO for implementation:
  ;; - [ ] Add database (Postgres).
  ;; - [ ] Daycare center communication is through an HTTP request.

  ())
