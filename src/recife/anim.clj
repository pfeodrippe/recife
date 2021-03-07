(ns recife.anim
  (:require
   [recife.core :as r]
   [quil.core :as q]
   [quil.middleware]))

;; Create a atom which can be changed with the result of some violation so we
;; don't need to recreate the `Quil` window.
(def ^:private anim-atom (atom {:result nil :sketch nil}))

(defmacro with-style
  [& body]
  `(try
     (q/push-style)
     ~@body
     (finally
       (q/pop-style))))

(defmulti draw-element (fn [v _] (type v)))

(defmethod draw-element clojure.lang.APersistentVector
  [coll {:keys [:x :y]}]
  (let [radius 10]
    (q/rect x y 80 20 radius radius radius radius)
    (with-style
      (q/fill 250)
      (->> coll
           (map-indexed (fn [idx _]
                          (q/ellipse (+ (* idx 20) 10 x) (+ 10 y) 10 10)))
           doall))))

(defmethod draw-element clojure.lang.APersistentSet
  [coll {:keys [:x :y]}]
  (let [radius 10]
    (q/rect x y 80 20 radius radius radius radius)
    ;; Draw white contour.
    (with-style
      (q/fill 250)
      (q/rect (+ x 1) (+ y 1) 78 18 radius radius radius radius)
      (q/fill 0)
      (q/rect (+ x 4) (+ y 4) 72 12 radius radius radius radius))
    (with-style
      (q/fill 250)
      (->> coll
           (map-indexed (fn [idx _]
                          (q/ellipse (+ (* idx 20) 10 x) (+ 10 y) 10 10)))
           doall))))

(defmethod draw-element :default
  [v {:keys [:x :y]}]
  (q/text (str v) x (+ y 13)))

(defn- setup
  []
  (q/frame-rate 10)
  (q/background 250)
  {:state-counter 0})

(defn- update'
  [{:keys [:paused?] :as old-state}]
  (let [result (:result @anim-atom)]
    (if (= (:trace result) :ok)
      (assoc old-state :result result)
      (let [state' (if paused? old-state (update old-state :state-counter inc))
            {:keys [:state-counter] :as state} (if (:reset? @anim-atom)
                                                 (do (swap! anim-atom assoc :reset? false)
                                                     (assoc state' :state-counter 0))
                                                 state')

            trace (:trace result)
            state-number (rem (int (/ state-counter (* 10 1.5))) (count trace))
            trace-state (second (get trace state-number))]
        (merge state
               {:state-number state-number
                :trace-state trace-state
                :result result
                :show-deadlock? (and (= state-number (dec (count trace)))
                                     (= (get-in result [:trace-info :violation :type]) :deadlock))
                :show-invariant? (and (= state-number (dec (count trace)))
                                      (= (get-in result [:trace-info :violation :type]) :invariant))
                :show-stuttering? (and (= state-number (dec (count trace)))
                                       (= (get-in result [:trace-info :violation :type]) :stuttering))
                :show-back-to-state? (and (= state-number (dec (count trace)))
                                          (= (get-in result [:trace-info :violation :type]) :back-to-state))})))))

(defn- draw-ok
  [_]
  (q/clear)
  (q/background 250)
  (q/stroke 20)
  (q/fill 0)

  ;; State number display.
  (with-style
    (q/text-font (q/create-font "Courier New" 15))
    (q/text "Everything is OK! No violation found."
            20
            30)))

(defn- draw*
  [{:keys [:paused? :state-number :trace-state :show-deadlock? :show-invariant?
           :show-back-to-state? :show-stuttering? :result]}]
  (q/clear)
  (q/background 250)
  (q/stroke 20)
  (q/fill 0)

  ;; State number display.
  (with-style
    (q/text-font (q/create-font "Courier New" 15))
    (q/text (format "State %s%s"
                    state-number
                    (cond
                      show-deadlock? " - Deadlock!"
                      show-invariant? " - Invariant Violated!"
                      show-back-to-state? (format " - Back to State %s!"
                                                  (get-in result [:trace-info :violation :state-number]))
                      show-stuttering? " - Stuttering!"
                      :else ""))
            20
            30))

  ;; Metadata context.
  (q/with-translation [150 70]
    (with-style
      (q/text-font (q/create-font "Courier New" 15))
      (q/text (-> trace-state :recife/metadata :context first str) 0 0)
      (q/text-font (q/create-font "Courier New" 13))
      (q/text (-> trace-state :recife/metadata :context second str) 0 20)))

  ;; Elements representing the variables.
  (q/with-translation [220 130]
    (->> (dissoc trace-state ::r/procs :recife/metadata)
         (sort-by first)
         (map-indexed (fn [idx [k v]]
                        (q/text (str k) 88 (+ (* idx 40) 13))
                        (draw-element v {:x 0 :y (* idx 40)})))
         vec))

  ;; Processes and their actual steps.
  (q/with-translation [20 500]
    (q/text-font (q/create-font "Courier New" 13))
    (->> (::r/procs trace-state)
         (map-indexed (fn [idx v]
                        (q/text (format "%s\t-->\t%s" (key v) (:pc (val v)))
                                0
                                (* idx 20))))
         vec))

  ;; Pause symbol.
  (when paused?
    (q/with-translation [550 20]
      (q/line 0 0 0 10)
      (q/line 4 0 4 10))))

(defn- draw
  [{:keys [:result] :as state}]
  (if (= (:trace result) :ok)
    (draw-ok state)
    (draw* state)))

(defn- key-released
  [state event]
  (case (:key event)
    :space
    (update state :paused? {false true true false nil true})

    :left
    (-> state
        (assoc :paused? true)
        (update :state-counter - 15)
        (update :state-counter max 0))

    :right
    (-> state
        (assoc :paused? true)
        (update :state-counter + 15))

    state))

(defn- on-close
  [_]
  (swap! anim-atom assoc :sketch nil))

(defn visualize-result
  [result]
  (swap! anim-atom assoc
         :result result
         :reset? true)
  (when (nil? (:sketch @anim-atom))
    (swap! anim-atom assoc :sketch
           (q/sketch
            :title "Recife Trace"
            :settings #(q/smooth 2)
            :setup #'setup
            :update #'update'
            :draw #'draw
            :size [600 600]
            :key-released #'key-released
            :on-close #'on-close
            :middleware [quil.middleware/fun-mode])))
  nil)

(comment

  ;; TODO:
  ;; - [x] First, let's just draw some representation for `:server/queue`.
  ;; - [ ] Create symbols (labels) for each step (maybe using first letters).
  ;; - [x] For a given process, show in which step we are.
  ;; - [x] For deadlock, show "Deadlock!" in the last state.
  ;; - [ ] Show which step acted to get to that step using the symbols.
  ;; - [ ] Relate the processes with `:server/queue` (using
  ;;       `:recife/metadata`).
  ;; - [ ] For a vector with more than 4 elements, show `...`. Maybe mouse hover
  ;;       will show more things about it? We can show expanded info in the REPL
  ;;       when the user clicks over some element.
  ;; - [x] Show state number.
  ;; - [ ] Create representation for other types (e.g. clojure.lang.APersistentMap).
  ;; - [ ] Create other visualizations, static and dynamic. Animation is not well
  ;;       suited for all outputs.

  ;; NOTES

  ;; You choose your variables of interest and `Recife` does the trace
  ;; around these.

  ;; You can see multiple quadrants, each which is a variable over time.

  ;; Real animations (videos) require less things on the screen, so maybe it's
  ;; more suitable to do this first. A static image is just  snapshots of the
  ;; animation between the transitions.

  ;; END OF NOTES.

  ())
