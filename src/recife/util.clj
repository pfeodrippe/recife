(ns recife.util
  (:require
   [taoensso.tufte :as tufte :refer [defnp p defnp-]]))

(defonce pd
  (tufte/new-pdata {:dynamic? true}))

(def enable-tufte false)
#_(def enable-tufte true)

(defmacro p*
  [id & body]
  (if enable-tufte
    `(do (tufte/add-basic-println-handler! {})
         (let [t0# (System/nanoTime)]
           (tufte/with-profiling pd {:dynamic? true}
             (try
               ~@body
               (catch Exception ex#
                 (println ex#)
                 (throw ex#))
               (finally
                 (tufte/capture-time! pd ~id (- (System/nanoTime) t0#)))))))
    `(do ~@body)))
