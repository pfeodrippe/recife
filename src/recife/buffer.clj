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

(set! *warn-on-reflection* true)

(defonce *channel-file
  (atom (File/createTempFile "my-file" ".txt")))

(defonce *contents
  (atom {}))

(defonce ^:private *new-contents?
  (atom false))

(defn buf-create
  ([]
   (buf-create {}))
  ([{:keys [file truncate]
     :or {file @*channel-file}}]
   (let [channel (FileChannel/open (.toPath ^File file)
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
  ([^java.nio.DirectCharBufferS buf]
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
    (not= (str (.get ^java.nio.DirectCharBufferS @*buf 0)) "1")
    (catch Exception ex
      (println "Exception in `can-write?`" ex)
      false)))

(defn- can-read?
  []
  (not (can-write?)))

(defn- buf-rewind
  ([]
   (buf-rewind @*buf))
  ([^java.nio.DirectCharBufferS buf]
   (.rewind buf)
   nil))

(def ^:private write-lock (Object.))

(defn- buf-write
  ([edn]
   (buf-write @*buf edn))
  ([buf edn]
   (buf-write buf edn {}))
  ([^java.nio.DirectCharBufferS buf edn {:keys [_offset]}]
   (locking write-lock
     (let [arr (.toCharArray (if edn
                               (str (pr-str edn) "\n")
                               (str (char 0))))]
       (.put buf arr)
       #_(if offset
           (.put buf arr offset (alength arr))
           (.put buf arr)))
     true)))

(def ^:private *saved (atom []))

(def ^:private flush-lock (Object.))

(defn flush!
  []
  (locking flush-lock
    (buf-write nil)
    (buf-rewind)
    (buf-write 1)
    (buf-rewind)
    (reset! *saved [])))

(defn- -save!
  [v]
  (locking lock
    (try
      (while (not (can-write?)))

      (when (zero? (.position ^java.nio.DirectCharBufferS @*buf))
        (buf-write 0))

      (buf-write v)
      (swap! *saved conj v)

      (when (= (count @*saved) 100)
        (flush!))

      (catch Exception ex
        (println ex)
        (throw ex)))))

(defn save!
  "Save value so it can be retrieved later. the value will be saved in a `bucket`.
  `bucket` is just a key (usually a keyword).

  If no `bucket` is passed, it stores data in a `:default` bucket."
  ([value]
   (save! :default value))
  ([bucket value]
   (try
     (p* ::save!
         (>!! @*client-channel [bucket value]))
     (catch Exception ex
       (if (nil? @*client-channel)
         (throw (ex-info "

Error when trying to save data, you need to set `:use-buffer` or
run Recife using `:generate`.

"
                         {}))
         (throw ex))))
   true))

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
           (swap! *contents (fn [contents]
                              (let [[bucket value] v]
                                (if (contains? contents bucket)
                                  (update contents bucket conj value)
                                  (update contents bucket (comp vec conj) value)))))
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
              #_(println "Starting sync loop...")
              (while (not (Thread/interrupted))
                (Thread/sleep 1)
                (try
                  (sync!)
                  (catch Exception e
                    (println e))))
              #_(println "STOPPING the future...")))))

#_(bean (type (clay/future pool (println "ss"))))

(defn stop-sync-loop!
  []
  (when-let [fut @*sync-loop]
    #_(println "Stopping sync loop...")
    (.cancel ^java.util.concurrent.Future fut true)
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
  ([]
   (read-contents :default))
  ([bucket]
   (reset! *new-contents? false)
   (if (= bucket :all)
     @*contents
     (get @*contents bucket))))

(defn has-new-contents?
  []
  @*new-contents?)
