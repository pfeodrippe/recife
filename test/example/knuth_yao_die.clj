(ns example.knuth-yao-die
  "Based on https://github.com/tlaplus/Examples/blob/master/specifications/KnuthYao/KnuthYao.tla.

  Also see https://www.youtube.com/watch?v=cYenTPD7740.

  See `example.knuth-yao-die.clerk` to see some charts."
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]))

(def states-map
  {:crooked {:h :s0 :t :s0}
   :s0 {:h :s1 :t :s2}
   :s1 {:h :s3 :t :s4}
   :s2 {:h :s5 :t :s6}
   :s3 {:h :s1 :t :1}
   :s4 {:h :2 :t :3}
   :s5 {:h :4 :t :5}
   :s6 {:h :6 :t :s2}})

;; Flag which control crookness.
(def crooked? false)

(def global
  {::state (if crooked? :crooked :s0)
   ::prob 1
   ::face (r/one-of #{:h :t})})

(def done #{:1 :2 :3 :4 :5 :6})

(r/defproc ^:fair next* {}
  {[::next
    {:next-face (fn [_]
                  (if-not crooked?
                    #{:h :t}
                    ;; Crooked coin.
                    (if (> (rand-int 8) 2)
                      #{:h}
                      #{:t})
                    ;; Another crooked coin.
                    ;; below. ~70% of the faces should be heads.
                    ;; Comment above `if` and uncomment below.
                    #_(if (> (rand-int 10) 2)
                        #{:h}
                        #{:t})))}]
   (fn [{:keys [::state ::prob ::face next-face] :as db}]
     (if (contains? done state)
       (r/done db)
       (-> db
           (assoc ::state (get-in states-map [state face])
                  ::prob (if (= state :crooked)
                           prob
                           (/ prob 2))
                  ::face next-face))))})

(rh/defconstraint eita
  [{:keys [::face ::state ::prob]}]
  (r/implies
   (contains? done state)
   (r/save! {:face face
             :state state
             :prob prob})))

(comment

  (def result
    (r/run-model global #{next* eita}
                 {:generate true
                  :depth 15}))

  (.close result)

  (count (r/read-saved-data))

  ;; 100000 in 30s

  (frequencies (r/read-saved-data))
  (frequencies (mapv :face (r/read-saved-data)))
  (frequencies (mapv :state (r/read-saved-data)))
  (into (sorted-map) (frequencies (mapv :prob (r/read-saved-data))))
  (take 500 (r/read-saved-data))

  ()

  ;; TODO:
  ;; - [x] Find a way to send a STOP command for simulate/generate flags
  ;; - [x] Send batches of 100 instead of only 1
  ;; - [x] Generate real-time charts with Clerk
  ;;   - [x] Use plotly
  ;;   - [x] Try to improve plotly perf by batching clerk/recompute!
  ;; - [x] Open PR for TLC with typo fixes
  ;; - [x] See how to pull from atom instead of pushing
  ;; - [x] Test crooked die

  ())
