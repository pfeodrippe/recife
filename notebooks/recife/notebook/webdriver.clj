;; ◊page-name[{:subtitle ""}]{webdriver}

;; This is more of an experiment to see what we can do with Recife
;; that's more in the unusual part. We will drive a browser using traces from a
;; specification.

;; Just to be clear, this is nothing new, we are just replicating what
;; ◊link{https://docs.quickstrom.io/en/latest/index.html}{Quickstrom}
;; has already done, check its
;; ◊link{https://docs.quickstrom.io/en/latest/tutorials/first.html}{tutorial},
;; that's where the HTML file and example come from.

(ns recife.notebook.webdriver
  {:nextjournal.clerk/visibility {:result :hide}
   :clerk/name "webdriver"}
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [is]]
   [recife.clerk :as rc]
   [recife.core :as r]
   [recife.helpers :as rh]
   [recife.webdriver :as rw]
   [wally.main :as w]))

;; ◊note{Check Quickstrom's
;; ◊link{https://docs.quickstrom.io/en/latest/tutorials/first.html}{original
;; tutorial}.}

;; ◊title{Audio Player}

;; We will be modelling the available actions in the
;; ◊link{https://github.com/pfeodrippe/recife/blob/master/resources-test/audioplayer.html}{audioplayer.html}
;; file.

;; For this webpage, the user can just click on a Play/Pause button
;; and see a clock (in the ◊code{00:00} format) evolving. See below.

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  :nextjournal.clerk/viewer 'nextjournal.clerk.viewer/html-viewer}
(slurp (io/resource "audioplayer.html"))

;; ◊note{A "variable" in our case is just a key in a map.}

;; The specification will be as simple as the audio player page, first
;; we will define a variable to keep track of the playing status and
;; another one to keep track of time (which may evolve when the status
;; is playing).

(def global
  {::playing? false
   ::time 0})

;; When not playing, the user can play.
(r/defproc play
  (fn [{::keys [playing?] :as db}]
    (when (not playing?)
      (assoc db ::playing? true))))

;; When playing, the user can pause.
(r/defproc pause
  (fn [{::keys [playing?] :as db}]
    (when playing?
      (assoc db ::playing? false))))

;; Time can advance when playing.
(r/defproc tick
  (fn [{::keys [playing?] :as db}]
    (when playing?
      (update db ::time inc))))

;; We also add a constraint so ◊code{time} does not evolve forever as it
;; can be potentially infinite.
(rh/defconstraint time-constraint
  [{::keys [time]}]
  (< time 10))

;; Then we have a pretty normal trace (if you don't understand
;; what's going one here, check the ◊xref{:doc/slow-start} and
;; ◊xref{:doc/temporal} guides).

^{:nextjournal.clerk/visibility {:result :show :code :hide}
  ::rc/id ::audio-player
  ::rc/block true}
(rc/run-model global #{play pause tick time-constraint} {:trace-example true})

;; ◊title{The driver}

;; With the specification above, we have some traces available for
;; us in the result. To drive a browser using these, we need a way to map the model
;; operators to real actions.

;; ◊em{But how the hell will I control the browser?}

;; ◊note{Yes, I like wrappers ! !}

;; I'm so glad you asked. Recently, I have been working on a wrapper
;; for Playwright, which is called
;; ◊link{https://github.com/pfeodrippe/wally}{Wally}. Playwright is a
;; webdriver in the same vein as Cypress and, I guess, Selenium. It's
;; available for different languages, including... Java, which Wally
;; leverages, but I will stop here, click
;; ◊link{https://github.com/pfeodrippe/wally}{here} to understand more
;; about the project.

;; ◊title{Mapping actions and variables}

;; From our specification, we have 3 possible actions:
;; ◊list{
;;   ◊code{play}, which translates to a click in the `.play-pause` class
;;
;;   ◊code{pause}, also translates to a click in the `.play-pause` class
;;
;;   ◊code{tick}, as long as the clock grows (or stays the same), it
;;   should do it, let's arbitrarily define it as a 1 second sleep
;;   (which will make our real clock change)
;; }

;; For the mapping, we will use... a map.
(def actions-mapping
  {::play
   (fn [_]
     (w/click :play-pause))

   ::pause
   (fn [_]
     (w/click :play-pause))

   ::tick
   (fn [_]
     (Thread/sleep 1000))})

;; We are not doing anything with the above map yet (we will use it in
;; our webdriver engine soon though). The operators from our
;; specification are the keys and the respective handlers are the
;; values. Each of these handlers receive one argument (which it's the
;; state of the trace that's driving it), but we don't care about it
;; for our current purposes.

;; We have actions, but we also have states, let's map them. Keys are
;; the variables and values are the respective check handlers (or
;; maps), read the comments below.
(def variables-mapping
  {::playing?
   ;; This is a check handler, it receives the old and current states
   ;; as arguments (note that it receives the entire `db`, not only
   ;; the variable used as key, so you have to destructure it).
   (fn [_ {::keys [playing?]}]
     ;; `playing?` comes from a specification trace state, it will
     ;; be `false` or `true`.
     (if playing?
       ;; If `true`, thee button text should be `Pause` ...
       (is (= "Pause" (w/text-content :play-pause)))
       ;; ... otherwise, `Play`.
       ;; Oh yeah, I almost forgot, using `is` from `clojure.test` is
       ;; optional, but it will enhance the error information.
       (is (= "Play" (w/text-content :play-pause)))))

   ;; For `::time`, we are using a map instead of a check handler, why?
   ;; Well, we can not only check the state, but also do a snapshot of the
   ;; real world variable so we can compare things.
   ;; Here we will compare the previous real clock with the current
   ;; one to make sure that they are always growing (or at least it's the
   ;; same).
   ::time
   {:check
    ;; `:check` is the check handler, the function has the same semantics as
    ;; the one used for `::playing?`.
    (fn [previous-db db]
      ;; `rw/impl-state` gets the stored implementation state, it's `nil`
      ;; if no snapshot occurred.
      (if-let [previous-time (-> previous-db rw/impl-state ::time)]
        (is (<= previous-time (-> db rw/impl-state ::time)))
        true))

    ;; Take a snapshot to be used in `:check`.
    :snapshot
    (fn [_]
      (let [[minutes seconds] (->> (-> (w/text-content :time-display)
                                       (str/split #":"))
                                   (mapv #(Integer/parseInt %)))]
        (+ (* 60 minutes) seconds)))}})

;; Wow, yeah, it's a lot, make sure you digest it well.

;; ◊title{Driving the player}

;; FINALLY we can use what we have created so far in the
;; ◊code{recife.webdriver/drive} function. This function makes the
;; specification control something (it's pretty agnostic), and this
;; something happens to be our HTML file in a browser.

;; ◊code{rw/drive} also expects a ◊code{:init} value which will be
;; used as the first step of the drive. We use it to navigate to our
;; static page.
(def init
  (fn  [_]
    (w/navigate (str (io/resource "audioplayer.html")))))

;; Then we drive it (open the REPL to see it in action)!
;; Or check the video at
;; ◊link{https://youtu.be/etHynX9IT9o}{https://youtu.be/etHynX9IT9o}.
(comment

  (rw/drive (r/get-result)
            {:init init
             :actions-mapping actions-mapping
             :variables-mapping variables-mapping
             :max-number-of-traces 1
             :max-number-of-states 10})

  ())

;; ◊title{Next steps}

;; ◊numbered-list{
;;   Being able to use temporal operators (e.g. eventually, always)
;;
;;   Create our own examples
;;
;;   Check the main limitations in comparions with Quickstrom
;; }

;; Bye \o
