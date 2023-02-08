(ns recife.clerk
  "You should have https://clojars.org/io.github.nextjournal/clerk and
  https://clojars.org/io.github.pfeodrippe/dev-tooling on the
  classpath to use this namespace.

  The functions require clerk on demand as we don't want to affect the startup
  time for a Recife model run."
  (:require
   [clojure.pprint :as pp]
   [com.pfeodrippe.tooling.clerk.util :as-alias tool.clerk.util]
   [nextjournal.clerk :as-alias clerk]
   [nextjournal.clerk.config :as clerk.config]
   [nextjournal.clerk.viewer :as-alias v]
   [recife.core :as-alias r]))

(defmacro with-recife
  [& body]
  (when (not (System/getProperty "RECIFE_OPTS_FILE_PATH"))
    (require '[com.pfeodrippe.tooling.clerk.util :as tool.clerk.util])
    (require '[nextjournal.clerk :as clerk])
    (require '[nextjournal.clerk.viewer :as v])
    (require '[recife.core :as r])
    `(do ~@body)))

(defmacro example
  [& body]
  (when clerk.config/*in-clerk*
    `(with-recife
       (clerk/with-viewer v/examples-viewer
         (mapv (fn [form# val#]
                 {:form form# :val val#})
               ~(mapv (fn [x#] `'~x#) body)
               ~(vec body))))))

(def recife-viewer
  (with-recife
    {:name ::recife-viewer
     :pred #(= (type %) ::r/RecifeResponse)
     :transform-fn (comp clerk/mark-presented
                         (clerk/update-val
                          (fn [result]
                            ;; Update trace elements so they contains the
                            ;; pretty-printed version that we can use for the
                            ;; node tooltip.
                            (update result :trace
                                    (fn [trace]
                                      (if (not (sequential? trace))
                                        trace
                                        (mapv (fn [[idx state]]
                                                [idx (assoc state :printed
                                                            (with-out-str
                                                              (pp/pprint
                                                               (dissoc state
                                                                       ::r/procs
                                                                       :recife/metadata))))])
                                              trace)))))))
     :render-fn
     '(fn [value]
        (v/html
         (when value
           [v/with-d3-require {:package ["elgrapho"]}
            (fn [elgrapho]
              (if (not (sequential? (:trace value)))
                [:div "No violation found =D"]
                (let [trace (:trace value)
                      model {:nodes
                             (->> trace
                                  (mapv (fn [[idx state]]
                                          {:x (+ 0 (* 0.1M idx))
                                           :y (+ 0 (* -0.1M idx))
                                           :group idx
                                           :label (if (= idx 0)
                                                    "0 - INITIAL"
                                                    (str idx
                                                         " - "
                                                         (-> (:recife/metadata state)
                                                             :context
                                                             first
                                                             name)))}))
                                  vec)

                             :edges
                             (->> trace
                                  (partition 2 1)
                                  (mapv (fn [[[idx-0 state-0]
                                              [idx-1 state-1]]]
                                          {:from idx-0
                                           :to idx-1})))}
                      build-graph
                      (fn [el]
                        (new elgrapho
                             (clj->js
                              {:model (-> model
                                          clj->js
                                          ((-> elgrapho
                                               .-layouts
                                               .-ForceDirected
                                               #_.-Hairball
                                               #_.-Cluster
                                               #_.-Chord)))
                               :container el
                               :width 700
                               :height 700
                               #_ #_:arrows true
                               #_ #_:glowBlend 0.2})))]
                  [:div {:ref (fn [el]
                                (when el
                                  (let [graph (build-graph el)]
                                    ;; Zoom out a little bit so we have room for the
                                    ;; nodes.
                                    (.zoomToPoint graph 0 0
                                                  (/ 1 1.1)
                                                  (/ 1 1.1))
                                    (set! (.-tooltipTemplate graph)
                                          (fn [idx el]
                                            (set! (.-innerHTML el)
                                                  (str "<pre>\n"
                                                       (:printed (last (get trace idx)))
                                                       (str "</pre>")))))
                                    graph)))}])))])))}))

(with-recife
  (tool.clerk.util/add-global-viewers!
   [recife-viewer]))

(comment

  (do
    (def value
      {:trace
       [[0
         {:recife.notebook.slow-start/hour 0,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}}}]
        [1
         {:recife.notebook.slow-start/hour 1,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [2
         {:recife.notebook.slow-start/hour 2,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [3
         {:recife.notebook.slow-start/hour 3,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [4
         {:recife.notebook.slow-start/hour 4,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [5
         {:recife.notebook.slow-start/hour 5,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [6
         {:recife.notebook.slow-start/hour 6,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [7
         {:recife.notebook.slow-start/hour 7,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [8
         {:recife.notebook.slow-start/hour 8,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [9
         {:recife.notebook.slow-start/hour 9,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [10
         {:recife.notebook.slow-start/hour 10,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [11
         {:recife.notebook.slow-start/hour 11,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [12
         {:recife.notebook.slow-start/hour 12,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [13
         {:recife.notebook.slow-start/hour 13,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [14
         {:recife.notebook.slow-start/hour 14,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [15
         {:recife.notebook.slow-start/hour 15,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [16
         {:recife.notebook.slow-start/hour 16,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [17
         {:recife.notebook.slow-start/hour 17,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [18
         {:recife.notebook.slow-start/hour 18,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [19
         {:recife.notebook.slow-start/hour 19,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [20
         {:recife.notebook.slow-start/hour 20,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [21
         {:recife.notebook.slow-start/hour 21,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [22
         {:recife.notebook.slow-start/hour 22,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]
        [23
         {:recife.notebook.slow-start/hour 23,
          :recife.core/procs
          #:recife.notebook.slow-start{:tick-v1
                                       {:pc
                                        :recife.notebook.slow-start/tick-v1}},
          :recife/metadata
          {:context
           [:recife.notebook.slow-start/tick-v1
            {:self :recife.notebook.slow-start/tick-v1}]}}]],
       :trace-info {:trace-example true},
       :distinct-states 24,
       :generated-states 25,
       :seed -1603827235550408286,
       :fp 25,
       :recife/transit-states-file-path
       "/var/folders/6r/vx4stgxd5yg6pbv_t_b2wttw0000gp/T/transit-output17294373035229478550.msgpack"})

    (def trace
      (:trace value)))

  ())
