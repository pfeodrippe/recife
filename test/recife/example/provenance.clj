(ns recife.example.provenance
  (:require
   [clojure.set :as set]
   [recife.core :as r]
   [recife.helpers :as rh]
   [clojure.core.logic :as l]))

#_(l/run* [q]
    (l/fresh [a]
      (l/membero a [1 4 3])
      (l/membero a [4 5 3])
      (l/membero a [7 8 q])))
;; => (4 3)

(def global
  {})

(r/defproc try-read
  (fn [{:keys [actor] :as db}]))

;; TODO:
;; - [ ] Can we find multiple examples using TLC?
;;   - [x] Save violations
;;   - [x] Action property
;;   - [ ] Temporal property
;;   - [ ] Invariants
;; - [ ] Maybe can use core.logic for the traces?
