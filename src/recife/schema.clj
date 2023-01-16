(ns recife.schema
  (:require
   [clojure.set :as set]
   [malli.core :as m]
   [malli.error :as me]))

(def Operator
  [:map
   [:recife.operator/type
    [:enum
     :tla-only
     :operator
     :invariant
     :state-constraint
     :action-constraint
     :temporal-property]]])

(def DefProc
  [:and [:cat
         symbol?

         [:map
          [:procs [:and [:set keyword?]
                   [:fn {:error/fn (fn [_ _] "should not be empty")}
                    seq]]]
          [:local [:map [:pc keyword?]]]]

         [:map-of
          [:or :keyword [:cat keyword? map?]]
          any?]]

   [:fn {:error/fn (fn [_ _] "`:pc` should be one of the steps")}
    (fn [[_ {:keys [:local]} steps]]
      ;; Initial `:pc` should exist in one of the step keys.
      (contains? (->> (keys steps)
                      (mapv (fn [v] (if (coll? v) (first v) v)))
                      set)
                 (:pc local)))]])

(def RunModelComponents
  [:and [:set any?]

   [:fn {:error/fn (fn [{:keys [value]} _]
                     (format "proc names should be unique: %s"
                             (mapv (comp set keys :procs) value)))}
    (fn [components]
      ;; Proc names should be unique, we check by counting them.
      (let [proc-names (mapv (comp set keys :procs) components)]
        (= (->> proc-names (apply set/union) count)
           (->> proc-names (map count) (apply +)))))]])

(defn explain-humanized
  [schema value error-message]
  (when-let [explained (m/explain schema value)]
    (throw (ex-info error-message
                    {:error-humanized (me/humanize explained)
                     :error explained}))))
