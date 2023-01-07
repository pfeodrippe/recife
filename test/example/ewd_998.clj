(ns example.ewd-998
  "Based on https://github.com/tlaplus/Examples/blob/master/specifications/KnuthYao/KnuthYao.tla.

  Also see https://www.youtube.com/watch?v=cYenTPD7740.

  See `example.knuth-yao-die.clerk` to see some charts."
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]
   [recife.buffer :as r.buf]))

(def nodes (set (range 2 #_3)))
(def colors #{:white :black})

(def global
  {::active (r/one-of (rh/combine nodes #{true false}))
   ::color (r/one-of (rh/combine nodes colors))
   ::counter (zipmap nodes (repeat 0))
   ::pending (zipmap nodes (repeat 0))
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

(r/defproc environment {}
  {[::send-msg
    {:i nodes
     :receiver nodes}]
   (fn [{::keys [active] :keys [i receiver] :as db}]
     (when (and (get active i)
                (not= i receiver))
       (-> db
           (update-in [::counter i] inc)
           (update-in [::pending receiver] inc))))

   [::recv-msg
    {:i nodes}]
   (fn [{::keys [pending] :keys [i] :as db}]
     (when (pos? (get pending i))
       (-> db
           (update-in [::pending i] dec)
           (update-in [::counter i] dec)
           (assoc-in [::color i] :black)
           (assoc-in [::active i] true))))

   [::deactivate
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

(comment

  (def result
    (r/run-model global #{initiate-probe pass-token
                          environment
                          state-constraint}
                 {:no-deadlock true
                  #_ #_:workers 1
                  #_ #_:generate true
                  #_ #_:depth 15}))

  (.close result)

  (count (r.buf/read-contents))

  (frequencies (r.buf/read-contents))
  (frequencies (mapv :face (r.buf/read-contents)))
  (frequencies (mapv :state (r.buf/read-contents)))
  (into (sorted-map) (frequencies (mapv :prob (r.buf/read-contents))))
  (take 500 (r.buf/read-contents))

  ()

  ;; TODO:
  ;; - [ ] Sets of functions
  ;; - [ ] Add spec for EWD998
  ;; - [ ] Add implicit `do` to helper macros
  ;;   - [ ] Improve args description
  ;; - [ ] Maybe add -noTE when running simulate/generate?

  ())
