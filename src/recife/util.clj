(ns recife.util
  (:require
   [taoensso.tufte :as tufte :refer [defnp p defnp-]]))

(defonce pd
  (tufte/new-pdata {:dynamic? true}))

(def enable-tufte
  #_false true)

(defmacro p*
  [id & body]
  (if enable-tufte
    `(do (tufte/add-basic-println-handler! {})
         (let [t0# (System/nanoTime)]
           (tufte/with-profiling pd {:dynamic? true}
             (try
               ~@body
               (finally
                 (tufte/capture-time! pd ~id (- (System/nanoTime) t0#)))))))
    `(do ~@body)))
