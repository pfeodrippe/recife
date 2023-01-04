(ns example.knuth-yao-die
  "Based on https://github.com/tlaplus/Examples/blob/master/specifications/KnuthYao/KnuthYao.tla.

  Also see https://www.youtube.com/watch?v=cYenTPD7740."
  {:nextjournal.clerk/visibility {:code :hide :result :hide}
   :nextjournal.clerk/no-cache true}
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]
   [recife.communication :as r.buf]
   [com.pfeodrippe.tooling.clerk :as tool.clerk]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.viewer :as viewer]))

;; ◊page-name[{:subtitle "6-sided die"}]{knuth yao}

(add-watch r.buf/*contents ::contents
           (fn [_k _r _old new]
             (when (zero? (mod (count new) 1000))
               (clerk/recompute!))))

^{::clerk/visibility {:result :show}}
(clerk/plotly {:data [{:values (let [{:keys [h t]
                                      :or {h 1 t 0}}
                                     (frequencies (mapv :face (r.buf/read-contents)))]
                                 [h t])
                       :labels ["h" "t"]
                       :type "pie"}]
               :layout {:height 300 :width 600}
               :config {:displayModeBar false
                        :displayLogo false}})

^{::clerk/visibility {:result :show}}
(clerk/plotly {:data [{:y (->> (frequencies (mapv :state (r.buf/read-contents)))
                               sort
                               (mapv second))
                       :x (mapv inc (range 6))
                       :type "bar"}]
               :layout {:height 600 :width 600}
               :config {:displayModeBar false
                        :displayLogo false}})


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

  (clerk/serve! {:watch-paths ["test/example"]})

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

  ;; TODO:
  ;; - [x] Find a way to send a STOP command for simulate/generate flags
  ;; - [x] Send batches of 100 instead of only 1
  ;; - [ ] Generate real-time charts with Clerk
  ;;   - [ ] Use plotly
  ;; - [ ] Add spec for EWD998
  ;; - [ ] Add implicit `do` to helper macros
  ;; - [ ] Maybe add -noTE when running simulate/generate?

  ())