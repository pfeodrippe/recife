^{:nextjournal.clerk/visibility {:code :hide}}
(ns recife.notebook.index
  {:nextjournal.clerk/no-cache true}
  (:require
   [nextjournal.clerk :as clerk]
   [com.pfeodrippe.tooling.clerk :as tool.clerk]))

{::clerk/visibility {:code :hide :result :hide}}

(tool.clerk/add-path-info!
 {:doc/reasoning 'recife.notebook.reasoning
  :doc/guide-layout 'recife.notebook.guide-layout
  :doc/non-determinism 'recife.notebook.non-determinism
  :doc/slow-start 'recife.notebook.slow-start})

{::clerk/visibility {:code :hide :result :show}}

;; ◊page-name[{:subtitle "A clojure model checker"}]{Recife Guide}

(tool.clerk/view-index
 [{:title "Intro"
   :pages [:doc/reasoning
           :doc/slow-start]}
  #_{:title "Concepts"
   :pages [:doc/non-determinism]}
  {:title "Appendix"
   :pages [:doc/guide-layout]}])

;; Check the ◊link{https://github.com/pfeodrippe/recife}{Github Repo}.

{::clerk/visibility {:code :hide :result :hide}}

(defn build!
  ([]
   (build! {}))
  ([{:keys [browse]
     :or {browse true}}]
   (let [exec! #(clerk/build! {:paths ["notebooks/**"]
                               :index "notebooks/recife/notebook/index.clj"
                               :bundle true
                               :browse %})]
     (when-not browse (exec! false))
     (exec! browse))))

(comment

  (clerk/serve! {:watch-paths ["notebooks"]})

  (build!)

  ;; TODO:
  ;; - [x] Use `dev-tooling`
  ;; - [x[ Serve docs using GH pages
  ;; - [x] Reasoning
  ;; - [x] Fix responsiveness
  ;; - [ ] Slow start
  ;; - [ ] Quick start
  ;; - [ ] Initial states
  ;;   - [ ] One of
  ;; - [ ] Proc
  ;; - [ ] Invariant
  ;; - [ ] Temporal property
  ;; - [ ] Action property
  ;; - [ ] Constraint
  ;; - [ ] Action constraint
  ;; - [ ] Tutorial
  ;; - [ ] Design
  ;;   - [ ] Implementation details
  ;; - [ ] Gather data for statistics
  ;; - [ ] Generate
  ;; - [ ] Simulation
  ;; - [ ] Model checking
  ;; - [ ] Non determinism
  ;; - [ ] Partner example
  ;;   - [ ] Driving implementation from traces
  ;;   - [ ] Reproducing bugs

  ;; TODO:
  ;; - [ ] Add search field
  ;; - [ ] Add inter-page ToC (on the left)
  ;; - [ ] Add on-page ToC (on the right)
  ;; - [ ] Add navigation between pages
  ;; - [ ] Diminish font size on mobile
  ;; - [ ] Fix flickering on phone
  ;;   -  [ ] Try to use the unbundled version of Clerk
  ;; - [ ] Copy code chunk to clipboard
  ;; - [ ] Add some emacs highlighting for ◊?

  ())
