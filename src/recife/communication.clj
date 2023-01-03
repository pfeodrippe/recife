(ns recife.communication
  (:require
   [clojure.edn :as edn])
  (:import
   (java.nio.channels FileChannel FileChannel$MapMode)
   (java.nio.file StandardOpenOption OpenOption)
   (java.io File)))

(defonce channel-file
  (File/createTempFile "my-file" ".txt"))

(defonce *contents
  (atom []))

(defn buf-create
  ([]
   (buf-create {}))
  ([{:keys [file truncate]
     :or {file channel-file}}]
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
         (edn/read-string line)

         :else
         (recur (.get buf) (str line c)))))))

(defn- can-write?
  []
  (not= (str (.get @*buf 0)) "1"))

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
   (let [arr (.toCharArray (str (pr-str edn) "\n"))]
     (if offset
       (.put buf arr offset (alength arr))
       (.put buf (.toCharArray (str (pr-str edn) "\n")))))
   true))

(defn save!
  [v]
  (locking lock
    (try
      (while (not (can-write?)))
      (buf-rewind)
      (buf-write 0)
      (buf-write v)
      (buf-rewind)
      (buf-write 1)
      (catch Exception ex
        (println ex)
        (throw ex)))))

(defn sync!
  []
  (boolean
   (when (can-read?)
     (buf-rewind)
     (let [v (do (buf-read-next-line)   ; discard first line
                 (buf-read-next-line))]
       (swap! *contents conj v))
     (buf-rewind)
     (buf-write 0))))

(defonce ^:private *sync-loop (atom nil))

(defn start-sync-loop!
  []
  (when-not @*sync-loop
    (reset! *sync-loop
            (future
              (while true
                (Thread/sleep 1)
                (try
                  (sync!)
                  (catch Exception e
                    (println e))))))))

(defn read-contents
  []
  @*contents)

(comment

  (doall
   (mapv #(save! {:g %}) (range 100)))

  (sync!)

  (read-contents)

  (count (read-contents))

  (can-read?)

  (save! {:a 30})

  (buf-read-next-line)
  (buf-rewind)

  (.position @*buf)

  (str channel-file)


  ())
