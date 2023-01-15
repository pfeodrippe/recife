(ns recife.buffer
  (:require
   [clojure.edn :as edn]
   [com.climate.claypoole :as clay]
   [clojure.core.async :as async :refer [go thread <! >! <!! >!! chan]]
   [recife.util :refer [p*]])
  (:import
   (java.nio.channels FileChannel FileChannel$MapMode)
   (java.nio.file StandardOpenOption OpenOption)
   (java.io File)))

(defonce *channel-file
  (atom (File/createTempFile "my-file" ".txt")))

(defonce *contents
  (atom []))

(defonce ^:private *new-contents?
  (atom false))

(defn buf-create
  ([]
   (buf-create {}))
  ([{:keys [file truncate]
     :or {file @*channel-file}}]
   (let [channel (FileChannel/open (.toPath file)
                                   (into-array OpenOption
                                               (concat [StandardOpenOption/READ
                                                        StandardOpenOption/WRITE
                                                        StandardOpenOption/CREATE]
                                                       (when truncate
                                                         [StandardOpenOption/TRUNCATE_EXISTING]))))
         buf (-> (.map channel FileChannel$MapMode/READ_WRITE 0 (* 4096 8 8 8 8))
                 .asCharBuffer)]
     buf)))

(defonce ^:private *buf
  (atom (buf-create)))

(defonce *client-channel
  (atom nil))

(defn reset-buf!
  []
  (reset! *channel-file (File/createTempFile "my-file" ".txt"))
  (reset! *buf (buf-create)))

(defn set-buf
  [buf]
  (reset! *buf buf))

(defonce ^:private lock
  (Object.))

(defonce ^:private lock-read
  (Object.))

(defn- buf-read-next-line
  ([]
   (buf-read-next-line @*buf))
  ([buf]
   (locking lock-read
     (loop [c (.get buf)
            line ""]
       (cond
         (zero? (int c))
         nil

         (or (= c \newline) (zero? (int c)))
         (try
           (edn/read-string line)
           (catch Exception ex
             (println "Cannot read line" {:line line})
             (throw ex)))

         :else
         (recur (.get buf) (str line c)))))))

(defn- can-write?
  []
  (try
    (not= (str (.get @*buf 0)) "1")
    (catch Exception ex
      (println "Exception in `can-write?`" ex)
      false)))

(defn- can-read?
  []
  (not (can-write?)))

(defn- buf-rewind
  ([]
   (buf-rewind @*buf))
  ([buf]
   (.rewind buf)
   nil))

(defn- buf-write
  ([edn]
   (buf-write @*buf edn))
  ([buf edn]
   (buf-write buf edn {}))
  ([buf edn {:keys [offset]}]
   (let [arr (.toCharArray (if edn
                             (str (pr-str edn) "\n")
                             (str (char 0))))]
     (if offset
       (.put buf arr offset (alength arr))
       (.put buf arr)))
   true))

(def ^:private *saved (atom []))

(defn- -save!
  [v]
  (locking lock
    (try
      (while (not (can-write?)))

      (when (zero? (.position @*buf))
        (buf-write 0))

      (buf-write v)
      (swap! *saved conj v)

      (when (= (count @*saved) 100)
        (buf-write nil)
        (buf-rewind)
        (buf-write 1)
        (buf-rewind)
        (reset! *saved []))

      (catch Exception ex
        (println ex)
        (throw ex)))))

(defn save!
  [v]
  (p* ::save!
      (>!! @*client-channel v))
  true)

(defonce ^:private lock-sync
  (Object.))

(defn sync!
  []
  (locking lock-sync
    (boolean
     (when (can-read?)
       (buf-rewind)
       ;; Discard first line.
       (buf-read-next-line)
       (loop [v (buf-read-next-line)]
         (when v
           (swap! *contents conj v)
           (recur (buf-read-next-line))))
       (when (seq @*contents)
         (reset! *new-contents? true))
       (buf-rewind)
       (buf-write 0)
       (buf-write nil)))))

(defonce ^:private *sync-loop (atom nil))

(defonce ^:private pool (clay/threadpool 2))

(defn start-sync-loop!
  []
  (when-not @*sync-loop
    (reset! *sync-loop
            (clay/future
              pool
              (println "Starting sync loop...")
              (while (not (Thread/interrupted))
                (Thread/sleep 1)
                (try
                  (sync!)
                  (catch Exception e
                    (println e))))
              (println "STOPPING the future...")))))

(defn stop-sync-loop!
  []
  (when-let [fut @*sync-loop]
    (println "Stopping sync loop...")
    (.cancel fut true)
    (reset! *sync-loop nil)
    (sync!)))

(defn start-client-loop!
  []
  (let [ch (chan 1000000)]
    (reset! *client-channel ch)
    (go
      (while true
        (let [v (<! ch)]
          (p* ::-save!
              (-save! v)))))))

(defn read-contents
  []
  (reset! *new-contents? false)
  @*contents)

(defn has-new-contents?
  []
  @*new-contents?)

(comment

  (doall
   (mapv #(save! {:g %}) (range 100)))

  (sync!)

  (read-contents)

  (count (read-contents))

  (can-read?)

  (save! {:a 30})

  (buf-rewind)
  (for [_ (range 100)]
    (buf-read-next-line))

  (buf-rewind)
  (loop [v (buf-read-next-line)
         x []]
    (if v
      (recur (buf-read-next-line)
             (conj x v))
      x))

  (do (.clear @*buf)
      true)

  (.position @*buf)

  (str @*channel-file)


  ())
