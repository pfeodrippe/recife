(ns example.knuth-yao-die
  "Based on https://github.com/tlaplus/Examples/blob/master/specifications/KnuthYao/KnuthYao.tla.

  Also see https://www.youtube.com/watch?v=cYenTPD7740."
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]
   [recife.communication :as r.buf]))

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
   ::probability 1
   ::face (r/one-of #{:h :t})})

(r/defproc ^:fair next* {}
  {[::next
    {:next-face #{:h :t}}]
   (fn [{:keys [::state ::probability ::face next-face] :as db}]
     (if (contains? #{:1 :2 :3 :4 :5 :6} state)
       (r/done db)
       (-> db
           (assoc ::state (get-in states-map [state face])
                  ::probability (/ probability 2)
                  ::face next-face))))})

(rh/defconstraint eita
  [{:keys [::face]}]
  (do
    (r.buf/save! #_{:a face
                  :b (repeat (rand-int 5) 4)} face)
    true))

(comment

  "7163 in 17s"
  "112400 in 16s"
  "89424 in 16s for complex"

  (count (r.buf/read-contents))
  (frequencies (r.buf/read-contents))
  (take 500 (r.buf/read-contents))

  (def result
    (r/run-model global #{next* eita}
                 {:workers 1
                  :generate true}))

  (.close result)

  ;; TODO:
  ;; - [x] Find a way to send a STOP command for simulate/generate flags
  ;; - [x] Send batches of 100 instead of only 1
  ;; - [ ] Generate real-time charts with Clerk
  ;; - [ ] Add spec for EWD998
  ;; - [ ] Add implicit `do` to helper macros
  ;; - [ ] Maybe add -noTE when running simulate/generate?

  ())
