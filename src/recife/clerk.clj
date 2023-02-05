(ns recife.clerk
  "You should have https://clojars.org/io.github.nextjournal/clerk on the
  classpath to use this namespace.

  The functions require clerk on demand as we don't want to affect the startup
  time for a Recife model run."
  (:require
   [nextjournal.clerk :as-alias clerk]
   [nextjournal.clerk.config :as clerk.config]
   [nextjournal.clerk.viewer :as-alias v]))

(defmacro with-recife
  [& body]
  (when (and (not (System/getProperty "RECIFE_OPTS_FILE_PATH"))
             nextjournal.clerk.config/*in-clerk*)
    `(do (require '[nextjournal.clerk :as clerk])
         (require '[nextjournal.clerk.viewer :as v])
         ~@body)))

(defmacro example
  [& body]
  `(with-recife
     (clerk/with-viewer v/examples-viewer
       (mapv (fn [form# val#]
               {:form form# :val val#})
             ~(mapv (fn [x#] `'~x#) body)
             ~(vec body)))))
