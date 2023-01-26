^{:nextjournal.clerk/visibility {:code :hide}}
(ns recife.notebook.index
  {:nextjournal.clerk/no-cache true}
  (:require
   [nextjournal.clerk :as clerk]
   [com.pfeodrippe.tooling.clerk :as tool.clerk]))

{::clerk/visibility {:code :hide :result :hide}}

(tool.clerk/add-path-info!
 {:doc/reasoning 'recife.notebook.reasoning})

{::clerk/visibility {:code :hide :result :show}}

;; ◊page-name[{:subtitle "Index"}]{Recife Guide}

(tool.clerk/view-index
 [{:title "Services"
   :pages [:doc/reasoning]}
  {:title "Appendix"
   :pages []}])

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
  ;; - [ ] Use `dev-tooling`
  ;; - [ ] Serve thing using GH pages
  ;; - [ ] proc
  ;; - [ ] reasoning
  ;; - [ ] invariant
  ;; - [ ] temporal property
  ;; - [ ] action property
  ;; - [ ] constraint
  ;; - [ ] action constraint
  ;; - [ ] tutorial
  ;; - [ ] design
  ;;   - [ ] implementation details
  ;; - [ ] save data

  ())
