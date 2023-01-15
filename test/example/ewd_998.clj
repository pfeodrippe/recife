(ns example.ewd-998
  "Based on https://github.com/tlaplus/Examples/blob/master/specifications/KnuthYao/KnuthYao.tla.

  Also see https://www.youtube.com/watch?v=cYenTPD7740.

  See `example.knuth-yao-die.clerk` to see some charts."
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def nodes (set (range 3)))
(def colors #{:white :black})

(def global
  {::active (r/one-of (rh/combine nodes #{true false}))
   ::color (r/one-of (rh/combine nodes colors))
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
                              (update :pos dec)
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
     (when (and (get active i)
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
  #_(and (every? #(and (<= (get counter %) 1)
                       (<= (get pending %) 1))
                 nodes)
         (<= (:q token) 9))
  (and (every? #(and (<= (get counter %) 3)
                     (<= (get pending %) 3))
               nodes)
       (<= (:q token) 9)))

(defn terminated?
  [{::keys [token counter color active pending]}]
  (= (zero? (:pos token))
     (= (:color token) :white)
     (zero? (+ (:q token) (get counter 0)))
     (= (get color 0) :white)
     (not (get active 0))
     (zero? (get pending 0))))

(comment

  ;; It takes ~23s|6s for 3 nodes, 1 worker, counter/pending <= 1.
  ;; 1m38s|1m05s for 3 nodes, :auto and counter/pending <= 3.
  (def result
    (r/run-model global #{initiate-probe pass-token
                          send-msg recv-msg deactivate
                          state-constraint}
                 {:no-deadlock true
                  :async true
                  #_ #_:workers 1
                  :seed 1
                  :fp 0
                  #_ #_     :trace-example? true
                  #_ #_:generate true
                  #_ #_:depth 15}))

  (.close result)
  @result

  ;; For statistics, see `EWD998_opts`.

  ;; TODO:
  ;; - [x] Sets of functions
  ;; - [x] Add spec for EWD998
  ;; - [x] Check why we have so few states
  ;; - [-] Check perf
  ;;   - It's not that easy, we need more profiling
  ;;   - Merge in `::r/result--merge` is slow
  ;; - [ ] Try to create KeywordValue
  ;; - [ ] Add statistics
  ;; - [ ] It seems that adding `currentState` slow things down
  ;;   - [ ] Maybe the field could be non-static?
  ;; - [ ] If you are starting a new Recife run, destroy any previous async runs
  ;; - [ ] See if exceptions are being thrown
  ;; - [ ] Add a way to for users plugins
  ;;   - [ ] Function that's run before and after the call
  ;;   - [ ] Function that's run wrapping the call
  ;; - [ ] Add implicit `do` to helper macros
  ;;   - [ ] Improve args description
  ;; - [ ] Maybe add -noTE when running simulate/generate?
  ;; - [ ] How to improve simulation of a step?
  ;; - [ ] Visualize trace with Clerk
  ;; - [ ] Could we create a custom class implementing `Value`?

  ())
