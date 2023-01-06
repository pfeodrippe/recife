(ns example.knuth-yao-die.clerk
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require
   [recife.buffer :as r.buf]
   [nextjournal.clerk :as-alias clerk]
   com.pfeodrippe.tooling.clerk))

;; ◊page-name[{:subtitle "6-sided die"}]{knuth yao}

^{::clerk/sync true}
(defonce *fe-data (atom {}))

^{::clerk/visibility {:result :show}}
(clerk/plotly {:data [{:values (let [{:keys [h t]
                                      :or {h 1 t 0}}
                                     (:faces @*fe-data)]
                                 [h t])
                       :labels ["h" "t"]
                       :type "pie"}]
               :layout {:height 300 :width 600}
               :config {:displayModeBar false
                        :displayLogo false}})

^{::clerk/visibility {:result :show}}
(clerk/plotly {:data [{:y (:states @*fe-data)
                       :x (mapv inc (range 6))
                       :type "bar"}]
               :layout {:height 600 :width 600}
               :config {:displayModeBar false
                        :displayLogo false}})

(defonce looper
  (future
    (while (not (Thread/interrupted))
      (Thread/sleep 100)
      (when (r.buf/has-new-contents?)
        (let [contents (r.buf/read-contents)]
          (reset! *fe-data {:faces (frequencies (mapv :face contents))
                            :states (->> (frequencies (mapv :state contents))
                                         sort
                                         (mapv second))}))))))

(comment

  (clerk/serve! {:watch-paths ["test/example/knuth_yao_die"]})

  (future-cancel looper)

  ())
