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
  (:require [recife.core :as r]
            [clojure.set :as set]))

(def global
  ;; All global keywords should be namespaced so we can differentiate it
  ;; from local variables in our processes.
  {
   ;; `:c1` and `:c2` are the companies.
   ;; `:token` is just some unique global identifier.
   ;; `:children` are the children of that company.
   ::accounts {:c1 {:token 0
                    :children #{:c2}}
               :c2 {:token 1}}

   ;; In the real world, we talk to our partner through a HTTP request, but here
   ;; we don't need to bother about implementation details like status, HTTP library
   ;; or error handling. We will model a API request by putting a element in a set,
   ;; which the partner process will consume.
   ::to-partner #{}})

;; TODO: Maybe for processes which only have one step, we can get rid of
;; this verbosity. The same for when there is only one process, we just pass
;; a empty `params` and it's fine, names are taken from `name`.
(r/defproc initial-request {}
  {::initial-request
   (fn [{:keys [::accounts] :as db}]
     ;; Initially, send only the companies with no children as there is no
     ;; `:id` that we can reference them in their parents.
     (let [companies-no-children (->> accounts
                                      (remove (comp seq :children val))
                                      keys
                                      set)]
       (-> db
           (update ::to-partner set/union companies-no-children)
           ;; We want this step to happen only in the beginning, so we will "close" it
           ;; with `r/done`.
           (r/done))))})

(comment

  ;; You can ask to generate a trace example (if a violation occurs, we will
  ;; return the violation as we normally do, the trace eaxample only happens
  ;; if every check was ok).
  (-> (r/run-model global #{initial-request} {:gen-trace-example? true})
      r/states-from-result)

  ;; TODO for implementation:
  ;; - [ ] Add database (Postgres).
  ;; - [ ] Daycare center communication is through an HTTP request.

  ())
