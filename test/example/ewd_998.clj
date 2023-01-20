(ns example.ewd-998
  "See https://www.youtube.com/watch?v=cYenTPD7740."
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def feature-flags #{:generate #_:pt3})

;; `rh/with-features` is used to make conditionals at compilation time.
(def nodes (set (range (rh/with-features feature-flags
                         :generate 43
                         3))))
(def colors #{:white :black})

(def global
  {::active (r/one-of (take 1000 (rh/combine nodes #{true false})))
   ::color (r/one-of (take 1000 (rh/combine nodes colors)))
   ::counter (repeat (count nodes) 0)
   ::pending (repeat (count nodes) 0)
   ::token {:pos 0 :q 0 :color :black}})

(r/defproc initiate-probe {}
  (fn [{::keys [token color counter] :as db}]
    (when (and (zero? (:pos token))
               (or (= (:color token) :black)
                   (= (get color 0) :black)
                   (pos? (+ (get counter 0)
                            (:q token)))))
      (-> db
          (assoc ::token {:pos (dec (count nodes))
                          :q 0
                          :color :white})
          (assoc-in [::color 0] :white)))))

(r/defproc pass-token {}
  {[::pass-token
    {:i (disj nodes 0)}]
   (fn [{::keys [active token color counter] :keys [i] :as db}]
     (when (and (not (get active i))
                (= (:pos token) i))
       (-> db
           (assoc ::token (-> token
                              (update :pos (rh/with-features feature-flags
                                             :pt3 #(if (= (get color i) :black)
                                                     0
                                                     (dec %))
                                             dec))
                              (update :q + (get counter i))
                              (assoc :color (if (= (get color i) :black)
                                              :black
                                              (:color token)))))
           (assoc-in [::color i] :white))))})

(r/defproc send-msg {}
  {[::send-msg
    {:i nodes
     :receiver nodes}]
   (fn [{::keys [active] :keys [i receiver] :as db}]
     (when (and
            ;; Just like it's done in EWD998.tla, we decrease the probability
            ;; of an active node send message over time. From what I see, this
            ;; is done so we don't have more well behaved T2TD.
            ;; This seems to be essential so we have well behaved traces.
            (rh/with-features feature-flags
              :generate (= (rand-int (+ 1 (rh/get-level)))
                           1)
              true)
            (get active i)
            (not= i receiver))
       (-> db
           (update-in [::counter i] inc)
           (update-in [::pending receiver] inc))))})

(r/defproc recv-msg {}
  {[::recv-msg
    {:i nodes}]
   (fn [{::keys [pending] :keys [i] :as db}]
     (when (pos? (get pending i))
       (-> db
           (update-in [::pending i] dec)
           (update-in [::counter i] dec)
           (assoc-in [::color i] :black)
           (assoc-in [::active i] true))))})

(r/defproc deactivate {}
  {[::deactivate
    {:i nodes}]
   (fn [{::keys [active] :keys [i] :as db}]
     (when (get active i)
       (-> db
           (assoc-in [::active i] false))))})

(rh/defconstraint state-constraint
  [{::keys [counter pending token]}]
  (and (every? #(and (<= (get counter %) 3)
                     (<= (get pending %) 3))
               nodes)
       (<= (:q token) 9)))

(defn- termination-detected?
  [{::keys [token counter color active pending]}]
  (= (zero? (:pos token))
     (= (:color token) :white)
     (zero? (+ (:q token) (get counter 0)))
     (= (get color 0) :white)
     (not (get active 0))
     (zero? (get pending 0))))

(defn- terminated?
  [{::keys [active pending]}]
  (and (every? false? active)
       (every? zero? pending)))

;; Action constraints are like normal constraints (state constraints), but you
;; can also retrieve information about the successor state (db').
(rh/defaction-constraint at-termination
  [db db']
  (when (not= (terminated? db)
              (terminated? db'))
    (rh/set-trace-value! :level (rh/get-level)))
  true)

(rh/defconstraint at-termination-detected
  [db]
  (when (termination-detected? db)
    (let [level (or (rh/get-trace-value :level) 0)
          t2td (- (rh/get-level) level)]
      (r/save! {:time-to-termination-detection t2td})
      (rh/set-trace-value! :level 0)))
  true)

(comment

  ;; It takes ~23s|6s for 3 nodes, 1 worker, counter/pending <= 1.
  ;; 1m38s|1m05s (56s after removing tla-edn.core binding) for 3 nodes, :auto and counter/pending <= 3.
  (def result
    (r/run-model global (concat
                         #{initiate-probe pass-token
                           send-msg recv-msg deactivate}
                         (rh/with-features feature-flags
                           :generate #{at-termination at-termination-detected}
                           #{state-constraint}))
                 (merge
                  {:no-deadlock true
                   :async true
                   #_ #_:workers 1
                   #_ #_:trace-example? true
                   :depth -1
                   ;; If you are using :generate, :use-buffer is
                   ;; set automatically.
                   #_ #_:use-buffer true}
                  (rh/with-features feature-flags
                    :generate {:generate {:num 10}}
                    {}))))

  (.close result)
  @result

  (count (r/read-saved-data))
  (into (sorted-map)
        (frequencies (mapv :time-to-termination-detection (r/read-saved-data))))

  (/ (double (apply + (mapv :time-to-termination-detection (r/read-saved-data))))
     (count (r/read-saved-data)))

  ;; For statistics, see `EWD998_opts`.

  ;; TODO:
  ;; - [x] Sets of functions
  ;; - [x] Add spec for EWD998
  ;; - [x] Check why we have so few states
  ;; - [x] Could we create a custom class implementing `Value`?
  ;; - [-] Check perf
  ;;   - It's not that easy, we need more profiling
  ;;   - Merge in `::r/result--merge` is slow
  ;; - [x] Add statistics
  ;;   - [x] Get current level
  ;;   - [x] Attach arbitrary data to a trace (like TLCSet)
  ;;   - [x] Add action constraint helper
  ;;   - [x] Add action constraint for `AtTermination`
  ;;     - [x] We will want to receive the next state
  ;;       - If required by the user, but the user may pass a function that,
  ;;         for example, accepts 2 args, the second one could be the next state
  ;;   - [x] Check why we are unable to generate meaningful lengths
  ;;     - I was using = instead of not= for `at-termination`
  ;;   - [-] It seems that we need to work on the perf to make this work properly :(
  ;;     - We are ~10x slower
  ;;     - [x] See the operations that are taking most of the time
  ;;       - Just computing the initial states take some time
  ;;   - [x] Check if adding support for RecifeEdnValue directly in TLC can help
  ;;     - No, it doesn't help much
  ;;   - [-] Generate combinations in TLC instead of in Clojure so it's more performant
  ;;     - [-] Add elisions when printing?
  ;;     - [-] Get random subset
  ;;     - Decided to just shuffle part of `rh/combine`
  ;;   - [x] Generate T2TD
  ;;   - [x] PT3
  ;;   - [x] Just profile again a little bit
  ;;   - [-] Clerk
  ;;     - We don't really need it, we know that it works based on the die example
  ;;   - [-] Organize options for the different scenarions better, they are just
  ;;         data. E.g. pt0 vs pt1 etc
  ;;     - Nah, let's not invest our time here, let's move on
  ;; - [x] Maybe add -noTE when running simulate/generate?
  ;; - [ ] If you are starting a new Recife run, destroy any previous async runs
  ;; - [ ] Add a way to for users plugins
  ;;   - [ ] Function that's run before and after the call
  ;;   - [ ] Function that's run wrapping the call
  ;; - [x] Add implicit `do` to helper macros
  ;;   - [ ] Improve args description
  ;; - [ ] How to improve simulation of a step?
  ;; - [ ] Export clj-kondo config
  ;; - [ ] As things are just functions, you can create tests for them
  ;;   - [ ] `terminated?`
  ;; - [ ] Add Recife Hooks?
  ;;   - [ ] Start
  ;;   - [ ] Each call
  ;; - [ ] Fix trace-example
  ;;   - [ ] Add a test for it
  ;;   - [ ] We may also want to generate a trace example even when we close a
  ;;         run
  ;; - [ ] Mix Recife with Datalog (Soufflé)?

  ())
