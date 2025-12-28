(ns example.tx-elle
  "We show here that we can leverage any Java/Clojure library with
  `Recife`, even complex ones like `Elle`."
  (:require
   [elle.list-append :as elle-append]
   [recife.core :as r]
   [recife.anim :as ra]
   [recife.helpers :as rh]))

(def global
  "`:tx/writes` is used to pass data from the `::client` process
  to `::write` in `:t1` or `:t2` (non-deterministic, `Recife` will test
  all the possibilities calling tje `TLC` model checker behind the scenes).
  `:tx/reads` has the already written values (entire history)."
  {:tx/writes []
   :tx/reads []})

(r/defproc tx {:procs #{:t1 :t2}     ; Two processes, `:t1` and `:t2`.
               :local {:pc ::write ; Start at `::write` step, `:pc` is required.
                       :queue []}  ; Local variable.
               }
  {::write
   (fn [{:keys [:tx/writes] :as db}]
     ;; We write to the local queue only, `Elle` should give us a counter
     ;; example for it as the other process will not have this value replicated
     ;; and `::client` is able to read from there.
     ;; If we don't have any received data (`seq` returns `nil` if it's argument
     ;; is empty), the process does nothing.
     (when-let [message (->> writes seq first)]
       (-> db
           (update :queue conj message)
           (update :tx/writes rest)
           ;; `r/goto` send the process to the next step, if we don't set it,
           ;; it would be stuck in `::write` forever.
           (r/goto ::read))))

   ::read
   (fn [{:keys [:queue] :as db}]
     (-> db
         (assoc :tx/reads queue) ; It can read `:queue` from `:t1` or `:t2`.
         (r/goto ::write)))})

;; `client` acts as a external process which doesn't have knowledge of the
;; internals of the database, `Elle` is a client-centric library.
(r/defproc client {:procs #{:client}
                   :local {:pc ::client-write
                           :to-be-sent 0}}
  ;; All namespaces in the steps (each `key` in map below) should be unique
  ;; globally, e.g. why we use `::client-write` instead of `::write` here
  ;; (which is already used in `tx`).
  {::client-write
   (fn [{:keys [:tx/writes :to-be-sent] :as db}]
     (when (empty? writes)              ; Do not write until message is consumed.
       (-> db
           (update :tx/writes conj to-be-sent)
           (update :to-be-sent inc)
           ;; We append to `:history` in `:recife/metadata` the values that we
           ;; write and read, it's used by `Elle` in the invariant.
           (update-in [:recife/metadata :history] conj [[:append :x to-be-sent]])
           (r/goto ::client-read))))

   ::client-read
   (fn [{:keys [:tx/reads] :as db}]
     (-> db
         (update-in [:recife/metadata :history] conj [[:r :x reads]])
         (r/goto ::client-write)))})

(rh/definvariant serializable?
  [{:keys [:recife/metadata]}]
  (let [result (->> (:history metadata)
                    (mapv (fn [transaction] {:type :ok, :value transaction}))
                    (elle-append/check {:consistency-models [:serializable], :directory "out"}))]
    ;; We can return a boolean or a two-sized vector. For the last one, the first
    ;; is a boolean and the second one is date you want to appear in the
    ;; output (if the boolean  is `false`).
    [(:valid? result) result]))

(rh/defconstraint constraint
  [{:keys [:recife/metadata]}]
  ;; As the growing is unbounded, just check until we have 20 transactions.
  (<= (-> metadata :history count) 20))

(comment

  (def result (r/run-model global #{tx client serializable? constraint}))

  (ra/visualize-result result)

  ;; One example of violation, yours may differ.
  (->> [[[:append :x 0]]
        [[:r :x [0]]]
        [[:append :x 1]]
        [[:r :x [1]]]
        [[:append :x 2]]
        [[:r :x [1 2]]]
        [[:append :x 3]]
        [[:r :x [1 2]]]
        [[:append :x 4]]
        [[:r :x [1 2]]]]
       (mapv (fn [transaction] {:type :ok, :value transaction}))
       (elle-append/check {:consistency-models [:serializable], :directory "out"}))

  ())
