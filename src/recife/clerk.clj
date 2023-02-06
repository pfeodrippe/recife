(ns recife.clerk
  "You should have https://clojars.org/io.github.nextjournal/clerk and
  https://clojars.org/io.github.pfeodrippe/dev-tooling on the
  classpath to use this namespace.

  The functions require clerk on demand as we don't want to affect the startup
  time for a Recife model run."
  (:require
   [com.pfeodrippe.tooling.clerk.util :as-alias tool.clerk.util]
   [nextjournal.clerk :as-alias clerk]
   [nextjournal.clerk.config :as clerk.config]
   [nextjournal.clerk.viewer :as-alias v]
   [recife.core :as-alias r]))

(defmacro with-recife
  [& body]
  (when (not (System/getProperty "RECIFE_OPTS_FILE_PATH"))
    `(do (require '[com.pfeodrippe.tooling.clerk.util :as tool.clerk.util])
         (require '[nextjournal.clerk :as clerk])
         (require '[nextjournal.clerk.viewer :as v])
         (require '[recife.core :as r])
         ~@body)))

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
     :transform-fn clerk/mark-presented
     :render-fn
     '(fn [value]
        (v/html
         (when value
           [v/with-d3-require {:package ["elgrapho"]}
            (fn [elgrapho]
              (let [build-graph
                    (fn [el]
                      (new elgrapho
                           (clj->js
                            {:model (-> {:nodes
                                         (->> (:trace value)
                                              (mapv (fn [[idx state]]
                                                      {:x (+ 0 (* 0.1 idx))
                                                       :y (+ 0 (* -0.1 idx))
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
                                         (->> (:trace value)
                                              (partition 2 1)
                                              (mapv (fn [[[idx-0 state-0]
                                                          [idx-1 state-1]]]
                                                      {:from idx-0
                                                       :to idx-1})))}
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
                                                (str "node </br>2 " idx))))
                                  graph)))}]))])))}))

(with-recife
  (tool.clerk.util/add-global-viewers!
   [recife-viewer]))

(comment

  ())
