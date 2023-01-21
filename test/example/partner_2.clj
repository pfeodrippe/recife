(ns example.partner-2
  (:require
   [clojure.set :as set]
   [medley.core :as medley]
   [recife.anim :as ra]
   [recife.core :as r]
   [recife.helpers :as rh]))

(def global
  {::companies {:c1 {:children #{:c2 :c4}}
                :c2 {}
                :c3 {:children #{:c1}}
                :c4 {}}
   :partner/reqs #{}
   :webhook/reqs #{}
   :id/counter 0
   :partner/history []})

;; We are now testing a temporal property (see `defproperty` below),
;; which means that "something good will eventually occur" (https://en.wikipedia.org/wiki/Liveness#:~:text=Liveness%20guarantees%20are%20important%20properties,something%20bad%20does%20not%20occur%22.).

;; Each process of `Recife` can stutter (see https://learntla.com/temporal-logic/termination/),
;; the process will not run (or not run anymore) and it will be stopped (in
;; stuttering state). A real process can fail (data center dies, server is thrown
;; into the water etc) and so a process like `initial-request` can just stop
;; and you can't do anything about it and the "something good" will not eventually
;; occur, too bad.

;; But here we are assuming that this does not happen (if it happens in the real
;; world, then we probably will have bigger issues) and that the world is fair
;; enough that it will at least retry the request at some point in time. So we
;; can use the `:fair` keyword (here as a metadata of `initial-request`) to indicate
;; that to our process. There is also `:fair+`, but it's not widely used, learn
;; more about this in this great Hillel's article https://www.hillelwayne.com/post/fairness/.
(r/defproc ^:fair initial-request
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

;; We add fairness here too.
;; We add one more process here (`:p2`).
(r/defproc ^:fair partner-server {:procs #{:p1 :p2}
                                  :local {:pc :partner/server}}
  ;; For step with non deterministic choice, see comment at
  ;; `:webhook/send-to-partner` and then come back here.
  ;; Here we are using the value of the global variable `partner/reqs`
  ;; as our source of non-determinism.
  {[:partner/server
    {:req :partner/reqs}]
   (fn [{:keys [:req :id/counter ::companies] :as db}]
     (if (every? (comp :id val) companies)
       (r/done db)
       (when-let [company-name req]
         (-> db
             (update :partner/reqs set/difference #{company-name})
             (update :webhook/reqs conj [company-name counter])
             (update :partner/history conj [company-name counter])
             (update :id/counter inc)))))})

;; We add fairness here as well.
;; We add one more process here (`:w2`).
(r/defproc ^:fair webhook {:procs #{:w1 :w2}
                           :local {:pc :webhook/handle-request}}
  ;; Here we are using another way to pass the step, now with non determinism.
  ;; We fetch one request (not all of them) non deterministically, adding
  ;; a `:req` key to the main step function.
  ;; Note that we are using a two-sized vector here with the name of the
  ;; step as our key.
  {[:webhook/handle-request
    {:req :webhook/reqs}]
   (fn [{:keys [:req ::companies] :as db}]
     (if (every? (comp :id val) companies)
       (r/done db)
       (when-let [[company-name id] req]
         (-> db
             (update :webhook/reqs set/difference #{req})
             (assoc-in [::companies company-name :id] id)
             (r/goto :webhook/load-companies)))))

   ;; This is emulating a query made to the database, where the view can be
   ;; outdated in comparison with the stored data.
   :webhook/load-companies
   (fn [{:keys [::companies :global/locked?] :as db}]
     (when-not locked?
       (-> db
           (assoc :loaded-companies companies)
           ;; Use a global lock so we don't send duplicate history.
           (assoc :global/locked? true)
           (r/goto :webhook/send-to-partner))))

   :webhook/send-to-partner
   (fn [{:keys [:loaded-companies] :as db}]
     (let [companies-ready (->> loaded-companies
                                ;; Remove sent companies.
                                (medley/remove-vals :sent?)
                                (medley/remove-vals :id)
                                (medley/filter-vals (fn [{:keys [:children]}]
                                                      ;; We already sent companies with no children
                                                      ;; in our initial request.
                                                      (and children
                                                           (every? #(-> loaded-companies % :id) children))))
                                keys
                                set)]
       (if (empty? companies-ready)
         (-> db
             (assoc :global/locked? false)
             (r/goto :webhook/handle-request))
         (-> db
             ;; Indicate that these companies were sent.
             (update ::companies medley/deep-merge (->> companies-ready
                                                        (mapv #(vector % {:sent? true}))
                                                        (into {})))
             (update :partner/reqs set/union companies-ready)
             (assoc :global/locked? false)
             (r/goto :webhook/handle-request)))))})

(rh/definvariant no-partner-history-duplicates
  [{:keys [:partner/history]}]
  (= (->> history (map first) distinct count)
     (count history)))

;; All companies will have an `:id` in the end of everything.
;; We say that at some point in time (`eventually`) and forever
;; (`always`) the companies will have an id (which is our main goal).
(rh/defproperty all-companies-will-have-an-id
  [{:keys [::companies]}]
  (rh/eventually
   (rh/always
    (and (->> (vals companies)
              (every? :id))
         ;; Also check that the ids are in the correct order.
         (> (-> companies :c3 :id)
            (-> companies :c1 :id)
            (-> companies :c4 :id))
         (> (-> companies :c1 :id)
            (-> companies :c2 :id))))))

(comment

  (def result
    @(r/run-model global
                  #{initial-request partner-server
                    webhook no-partner-history-duplicates
                    all-companies-will-have-an-id}
                  {:trace-example? true}))

  ;; With these functions you can see the timeline diff (`:-` or `:+`) to see
  ;; what changed. `r/print-timeline-diff` prints the output to the STDOUT (REPL)
  ;; and highlights the differences, so it may be easier to debug larger traces
  ;; with it.
  (r/timeline-diff result)
  (r/print-timeline-diff result)

  (ra/visualize-result result)

  ;; TODO:
  ;; - [x] IMPORTANT! When a temporal property is added, the successor in the
  ;;       states file is not correct.
  ;; - [x] Add invariants.
  ;; - [x] Add temporal properties.
  ;; - [x] Add two partner and two webhook processes.

  ;; TODO for implementation:
  ;; - [ ] Implementation.

  ())
