(ns example.knuth-yao-die
  "Based on https://github.com/tlaplus/Examples/blob/master/specifications/KnuthYao/KnuthYao.tla.

  Also see https://www.youtube.com/watch?v=cYenTPD7740.

  See `example.knuth-yao-die.clerk` to see some charts."
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]
   [recife.buffer :as r.buf]))

(def states-map
  {:s0 {:h :s1 :t :s2}
   :s1 {:h :s3 :t :s4}
   :s2 {:h :s5 :t :s6}
   :s3 {:h :s1 :t :1}
   :s4 {:h :2 :t :3}
   :s5 {:h :4 :t :5}
   :s6 {:h :6 :t :s2}})

(def global
  {::state :s0
   ::prob 1
   ::face (r/one-of #{:h :t})})

(def done #{:1 :2 :3 :4 :5 :6})

(r/defproc ^:fair next* {}
  {[::next
    {:next-face #{:h :t}}]
   (fn [{:keys [::state ::prob ::face next-face] :as db}]
     (if (contains? done state)
       (r/done db)
       (-> db
           (assoc ::state (get-in states-map [state face])
                  ::prob (/ prob 2)
                  ::face next-face))))})

(rh/defconstraint eita
  [{:keys [::face ::state ::prob]}]
  (r/implies
   (contains? done state)
   (r.buf/save! {:face face
                 :state state
                 :prob prob})))

(comment

  (def result
    (r/run-model global #{next* eita}
                 {:workers 1
                  :generate true
                  :depth 15}))

  (.close result)

  (count (r.buf/read-contents))

  (frequencies (r.buf/read-contents))
  (frequencies (mapv :face (r.buf/read-contents)))
  (frequencies (mapv :state (r.buf/read-contents)))
  (into (sorted-map) (frequencies (mapv :prob (r.buf/read-contents))))
  (take 500 (r.buf/read-contents))

  ()

  ;; TODO:
  ;; - [x] Find a way to send a STOP command for simulate/generate flags
  ;; - [x] Send batches of 100 instead of only 1
  ;; - [x] Generate real-time charts with Clerk
  ;;   - [x] Use plotly
  ;;   - [x] Try to improve plotly perf by batching clerk/recompute!
  ;; - [x] Open PR for TLC with typo fixes
  ;; - [x] See how to pull from atom instead of pushing
  ;; - [ ] Test crooked die
  ;; - [ ] Add spec for EWD998
  ;; - [ ] Add implicit `do` to helper macros
  ;; - [ ] Maybe add -noTE when running simulate/generate?

  ())
