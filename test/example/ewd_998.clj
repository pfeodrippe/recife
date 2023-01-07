(ns example.ewd-998
  "Based on https://github.com/tlaplus/Examples/blob/master/specifications/KnuthYao/KnuthYao.tla.

  Also see https://www.youtube.com/watch?v=cYenTPD7740.

  See `example.knuth-yao-die.clerk` to see some charts."
  (:require
   [recife.core :as r]
   [recife.helpers :as rh]
   [recife.buffer :as r.buf]))

(def nodes (range 3))
(def colors #{:white :black})

(def global
  {::active (r/one-of (rh/combine nodes #{true false}))
   ::color (r/one-of (rh/combine nodes colors))
   ::counter (zipmap nodes (repeat 0))
   ::pending (zipmap nodes (repeat 0))
   ::token {:pos 0 :q 0 :color :black}})

(r/defproc initiate-probe {}
  (fn [{::keys [token color counter] :as db}]
    (when (and (zero? (:pos token))
               (or (= (:color token) :black)
                   (= (get color 0) :black)
                   (pos? (+ (get counter 0)
                            (:q token)))))
      (-> db
          (assoc ::token {:pos (dec (count nodes))
                          :q 0
                          :color :white})
          (assoc-in [::color 0] :white)))))

(r/defproc pass-token {}
  (fn [{::keys [token color counter] :as db}]
    #_(when)))



(comment

  (def result
    (r/run-model global #{next* eita}
                 {:workers 1
                  :generate true
                  :depth 15}))

  (.close result)

  (count (r.buf/read-contents))

  (frequencies (r.buf/read-contents))
  (frequencies (mapv :face (r.buf/read-contents)))
  (frequencies (mapv :state (r.buf/read-contents)))
  (into (sorted-map) (frequencies (mapv :prob (r.buf/read-contents))))
  (take 500 (r.buf/read-contents))

  ()

  ;; TODO:
  ;; - [ ] Sets of functions
  ;; - [ ] Add spec for EWD998
  ;; - [ ] Add implicit `do` to helper macros
  ;; - [ ] Maybe add -noTE when running simulate/generate?

  ())
