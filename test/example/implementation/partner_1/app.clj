(ns example.implementation.partner-1.app
  "Implementation of `example.partner-1`.

  Make sure components (e.g. db) are running, see README.

  We divide this namespace in sections so we don't have to create tiny
  namespaces where our code is scattered over multiple files."
  (:require
   [arrudeia.core :as ar]
   [clj-http.client :as http]
   [clj-http.fake :as http-fake]
   [clojure.edn :as edn]
   [example.partner-2 :as model]
   [honey.sql :as sql]
   [honey.sql.helpers :as h]
   [integrant.core :as ig]
   [muuntaja.core :as m]
   [next.jdbc :as jdbc]
   [next.jdbc.sql :as j-sql]
   [org.httpkit.server :refer [run-server]]
   [recife.core :as r]
   [reitit.ring :as ring]
   [reitit.ring.coercion :as rrc]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [medley.core :as medley]
   [clojure.set :as set]
   [recife.analyzer :as analyzer]))

;;;;;;;;;;;;;;;;;;;; PARTNER ;;;;;;;;;;;;;;;;;;;;

(defn partner-signup
  "Sign up a company into our partner."
  ([company]
   (partner-signup company []))
  ([{:keys [:company/name]} children]
   (http/request {:method :post
                  ;; For our sake, the identifier is just the company
                  ;; name.
                  :params (merge {:identifier name}
                                 (when (seq children)
                                   ;; We can only send children if they already
                                   ;; have the partner id (created by the partner,
                                   ;; we don't have control over this id).
                                   ;; This is why we have to sign up the children
                                   ;; first.
                                   {:children children}))
                  :url "http://partner.recife/signup"})))

;;;;;;;;;;;;;;;;;;;; HANDLERS ;;;;;;;;;;;;;;;;;;;;

(defn partner-send-companies-handler
  "Send companies with no children to partner.
  The partner will trigger the webhook."
  [{:keys [:db]}]
  (let [companies-no-children (->> (-> (h/select :*)
                                       (h/from :company)
                                       (h/where [:not-in
                                                 :company.id
                                                 (-> (h/select :parent_company_id)
                                                     (h/from :company)
                                                     (h/where [:not= :parent_company_id nil]))])
                                       sql/format)
                                   (jdbc/execute! db))]
    (ar/with-label ::send-companies-with-no-children
      (try
        (mapv partner-signup companies-no-children)
        (finally
          (mapv #(j-sql/update! db :company
                                {:was_sent_to_partner true}
                                {:id (:company/id %)})
                companies-no-children))))
    ;; Always return ok after sending children companies to the partner,
    ;; the interaction with it is asynchronous (via webhook).
    {:status 200
     :body "ok"}))

(defonce partner-webhook-lock (atom false))

(defn partner-webhook-handler
  "Called by the partner and if it's a event type from creation, it sets
  the partner id for a given company.
  Then it sends available parent companies (now that you know children ids) to
  be registered in the partner."
  [{:keys [:db :body-params]}]
  (let [{:keys [:event-type :company-identifier :id]} body-params]
    (when (= event-type "create")
      (ar/with-label ::handle-request
        (j-sql/update! db :company
                       {:partner_id id}
                       {:name company-identifier}))
      (loop [lock-acquired? (compare-and-set! partner-webhook-lock false true)]
        (when-not lock-acquired?
          (ar/with-label ::load-companies)
          (recur (compare-and-set! partner-webhook-lock false true))))
      (let [available-companies (->> (-> (h/select :parent.* [:child.partner_id :child_partner_id])
                                         (h/from [:company :parent])
                                         (h/join [:company :child] [:= :child.parent_company_id :parent.id])
                                         (h/where [:not-exists
                                                   (-> (h/select :child.partner_id)
                                                       (h/from [:company :child])
                                                       (h/where [:= :child.parent_company_id :parent.id]
                                                                [:= :child.partner_id nil]))]
                                                  [:= :parent.partner_id nil]
                                                  [:= :parent.was_sent_to_partner nil])
                                         sql/format)
                                     (jdbc/execute! db)
                                     (group-by :company/id))]
        (ar/with-label ::load-companies available-companies)
        (ar/with-label ::send-to-partner
          (try
            (mapv #(partner-signup (first (val %))
                                   (mapv :company/child_partner_id (val %)))
                  available-companies)
            (finally
              (mapv #(j-sql/update! db :company
                                    {:was_sent_to_partner true}
                                    {:id (key %)})
                    available-companies)
              (reset! partner-webhook-lock false))))))
    {:status 200
     :body "ok"}))

(comment

  (declare db)
  (declare app)

  (j-sql/find-by-keys db :company :all)

  ;; Reset partner ids.
  (j-sql/update! db :company {:partner_id nil} {:name "c4"})
  (j-sql/update! db :company {:partner_id nil} {:name "c2"})

  ;; Send companies.
  (http/request {:method :post
                 :headers {"fake-routes"
                           (pr-str
                            {"http://partner.recife/signup"
                             {:post
                              '(fn [_]
                                 {:status 200 :body "ok"})}})}
                 :url "http://localhost:3000/partner/send-companies"})


  ;; Webhook handler.
  (http/request {:method :post
                 :form-params {:event-type "create"
                               :company-identifier "c4"
                               :id 50}
                 :content-type :json
                 :url "http://localhost:3000/partner/webhook"})

  (http/request {:method :post
                 :form-params {:event-type "create"
                               :company-identifier "c2"
                               :id 51}
                 :content-type :json
                 :url "http://localhost:3000/partner/webhook"})

  (http/request {:method :post
                 :form-params {:event-type "create"
                               :company-identifier "c2"
                               :id 51}
                 :content-type :json
                 :url "http://localhost:3000/partner/webhook"})

  ;; DB data.
  (j-sql/find-by-keys db :company :all)

  ())

(defn routes
  []
  [[""
    ["/partner"
     ["/send-companies" {:post partner-send-companies-handler}]
     ["/webhook" {:post partner-webhook-handler}]]]])

(def middleware-inject-components
  {:name ::inject-components
   :compile (fn [{:keys [:components]} _]
              ;; Inject components to request map, (e.g. we can access
              ;; `:db` directly in the handler).
              (fn [handler]
                (fn [req]
                  (handler (merge req components)))))})

(def middleware-fake-routes
  "Should be used only for tests where you want to send fake routes
  over the wire."
  {:name ::fake-routes
   :compile (fn [_ _]
              (fn [handler]
                (fn [req]
                  (if-let [fake-routes (get-in req [:headers "fake-routes"])]
                    (http-fake/with-fake-routes (eval (edn/read-string fake-routes))
                      (handler req))
                    (handler req)))))})

(defn app
  [components]
  (ring/ring-handler
   (ring/router
    (routes)
    {:data {:components components
            :muuntaja m/instance
            :middleware [middleware-inject-components
                         middleware-fake-routes
                         muuntaja/format-middleware
                         rrc/coerce-exceptions-middleware
                         rrc/coerce-request-middleware
                         rrc/coerce-response-middleware]}})
   (ring/routes
    (ring/create-resource-handler
     {:path "/"})
    (ring/create-default-handler
     {:not-found (constantly {:status 404 :body "Not found"})}))))

;;;;;;;;;;;;;;;;;;;; SYSTEM ;;;;;;;;;;;;;;;;;;;;

(defmethod ig/init-key ::server
  [_ {:keys [:handler] :as opts}]
  (run-server handler (-> opts (dissoc handler) (assoc :join? false))))

(defmethod ig/halt-key! ::server
  [_ server]
  (server))

(defmethod ig/init-key :handler/run-app
  [_ {:keys [:db]}]
  (app {:db db}))

(defmethod ig/init-key :database.sql/connection
  [_ db-spec]
  (let [db (jdbc/get-datasource db-spec)]
    ;; Create company table here for convenience..
    (->> (-> (h/create-table :company :if-not-exists)
             (h/with-columns [[:id :serial [:primary-key]]
                              ;; `name` is our identifier, but in real world apps, we
                              ;; would use an Tax ID.
                              [:name [:varchar 128] [:not nil] :unique]
                              [:parent_company_id :int nil]
                              [:partner_id :int nil]
                              [:was_sent_to_partner :boolean nil]
                              [[:constraint :company_fk]
                               [:foreign-key :parent_company_id]
                               [:references [:company :id]]]])
             sql/format)
         (jdbc/execute! db))
    db))

(def config
  {::server {:handler (ig/ref :handler/run-app) :port 3000}
   :handler/run-app {:db (ig/ref :database.sql/connection)}
   :database.sql/connection {:dbtype "postgres"
                             :dbname "partner_1_db"
                             :password "example"
                             :user "postgres"
                             :port 5500}})

(defn -main []
  (ig/init config))

(do (when (some-> (resolve 'sys) bound?)
      (ig/halt! (var-get (resolve 'sys))))
    (def sys (-main))

    (def db (:database.sql/connection sys)))

(comment

  ;; DONE: Call fake routes over the wire.
  (http-fake/with-fake-routes {"http://partner.recife/signup"
                               {:post
                                (fn [_]
                                  {:status 200 :body "ok"})}}
    (-> (http/request {:method :post
                       :url "http://localhost:3000/partner/send-companies"})))

  (->> (-> (h/drop-table :company) sql/format)
       (jdbc/execute! db))

  (->> (-> (h/truncate :company) sql/format)
       (jdbc/execute! db))

  (j-sql/find-by-keys db :company :all)

  (j-sql/insert-multi! db :company
                       [:name]
                       [["Olha"]])

  (j-sql/insert-multi! db :company
                       [:name :parent_company_id]
                       [["Eita" (:company/id (first (j-sql/find-by-keys db :company {:name "Olha"})))]])

  (j-sql/insert-multi! db :company
                       [:name :parent_company_id]
                       [["Abrabo" (:company/id (first (j-sql/find-by-keys db :company {:name "Olha"})))]])

  (http-fake/with-fake-routes {"http://partner.recife/signup"
                               {:post
                                (fn [_]
                                  {:status 200 :body "ok"})}}
    ((app {:db db}) {:request-method :post, :uri "/partner/send-companies"}))

  ;; TODO:
  ;; - [x] Send initial data to partner.
  ;; - [x] Use trace for sending requests to webhook.
  ;; - [x] Webhook.
  ;; - [x] Send parent companies.
  ;; - [x] Check trace.
  ;; - [x] Create a counterexample.

  ())

;; No violation found.
(declare trace)

;; Trace with some counterexample (violation).
(declare c-trace)

(comment

  ;; TODO: Make it work for HTTP request so it's not
  ;; bypassed. We have to make the control serializable from
  ;; the wire somehow. For it we could have the arrudeia state
  ;; in some storage (database or just atoms backed by files).
  #_(http/request {:method :post
                   :headers {"fake-routes"
                             (pr-str
                              {"http://partner.recife/signup"
                               {:post
                                '(fn [x]
                                   {:status 200 :body "ok"})}})}
                   :url "http://localhost:3000/partner/send-companies"})

  (j-sql/find-by-keys db :company :all)

  (def result
    (r/run-model model/global
                 #{model/initial-request
                   model/partner-server
                   model/webhook
                   model/no-partner-history-duplicates
                   model/all-companies-will-have-an-id}
                 {:trace-example? true}))

  ;; We cannot guarantee that the implementation is a refinement of the model,
  ;; but we can say that it satisfies the model. One way to generate a counterexample
  ;; is to deliberately create a trace which violates some invariant or property
  ;; so it can be used in one of our tests.
  (let [
        ;; Receives initial global state from a trace.
        init
        (fn [{:keys [::model/companies]}]
          ;; Reset lock.
          (reset! partner-webhook-lock false)
          ;; Remove all records for every new trace.
          (->> (-> (h/truncate :company) sql/format)
               (jdbc/execute! db))
          ;; Insert all the companies according to initial global state.
          (->> companies
               (mapv (fn [[identifier]] [(name identifier)]))
               (j-sql/insert-multi! db :company [:name]))
          ;; Set parents for each child.
          (->> companies
               (mapv (fn [[identifier {:keys [:children]}]]
                       (mapv #(j-sql/update! db :company
                                             {:parent_company_id
                                              (-> (j-sql/find-by-keys db :company
                                                                      {:name (name identifier)})
                                                  first
                                                  :company/id)}
                                             {:name (name %)})
                             children)))))

        ;; Mapping from model step to implementation lifecycle.
        implementation-mapping
        {::model/initial-request
         {:step ::send-companies-with-no-children
          :register (fn [_]
                      (http-fake/with-fake-routes {"http://partner.recife/signup"
                                                   {:post
                                                    (fn [req]
                                                      {:req req :status 200 :body "ok"})}}
                        ((app {:db db})
                         {:request-method :post
                          :uri "/partner/send-companies"})))
          :handler (fn [{:keys [:step-result]}]
                     (->> step-result
                          (mapv #(-> % :req :params :identifier keyword))))
          ;; `:assertion` will be compared step by step, it's a optional parameter.
          :assertion (fn [{:keys [:output]}]
                       {:expected #{:c4 :c2}
                        :actual (set output)})}

         ;; `::model/partner-server` is one step we don't have
         ;; control of, the code for this is in the partner
         ;; server and we will use the trace state to
         ;; call the webhook.
         :partner/server
         {:step ::partner-server}

         :webhook/handle-request
         {:step ::handle-request
          ;; Guard for `:register` so it does not register something which is not
          ;; going to be used.
          :guard (fn [{:keys [:context]}] (some? (:req context)))
          :register (fn [{:keys [:context]}]
                      (let [[company-identifier partner-id] (:req context)]
                        (http-fake/with-fake-routes {"http://partner.recife/signup"
                                                     {:post
                                                      (fn [req]
                                                        {:req req :status 200 :body "ok"})}}
                          ((app {:db db})
                           {:request-method :post
                            :uri "/partner/webhook"
                            :body-params {:event-type "create"
                                          :company-identifier (name company-identifier)
                                          :id partner-id}}))))
          ;; `:handler` can be used with register so you can, for example,
          ;; get a snapshot of the database after the action was done like
          ;; the function below.
          :handler (fn [_]
                     (->> (j-sql/find-by-keys db :company :all)
                          (mapv (juxt (comp keyword :company/name)
                                      #(-> %
                                           (select-keys [:company/partner_id :company/was_sent_to_partner])
                                           (set/rename-keys {:company/partner_id :id
                                                             :company/was_sent_to_partner :sent?}))))
                          (into {})))
          :assertion (fn [{:keys [:model-state :output]}]
                       {:expected (->> (::model/companies model-state)
                                       (medley/map-vals #(merge {:id nil :sent? nil}
                                                                (select-keys % [:id :sent?]))))
                        :actual output})}

         :webhook/load-companies
         {:step ::load-companies
          :handler (fn [_]
                     (->> (j-sql/find-by-keys db :company :all)
                          (mapv (juxt (comp keyword :company/name)
                                      #(-> %
                                           (select-keys [:company/partner_id :company/was_sent_to_partner])
                                           (set/rename-keys {:company/partner_id :id
                                                             :company/was_sent_to_partner :sent?}))))
                          (into {})))}

         :webhook/send-to-partner
         {:step ::send-to-partner
          :handler (fn [{:keys [:step-result]}]
                     (->> step-result
                          (mapv #(-> % :req :params :identifier keyword))))
          ;; `:reload?` is used for the last step in a process if you want to indicate
          ;; that step should be cancelled (would be fresh next time it's called).
          :reload? true
          :assertion (fn [{:keys [:model-state :model-previous-state :output]}]
                       {:expected (:partner/reqs model-state)
                        :actual (set/union (:partner/reqs model-previous-state) (set output))})}}]
    (analyzer/analyze result
                      {:init init
                       :number-of-traces 10
                       :implementation-mapping implementation-mapping}))

  ;; TODO (PRIORITY):
  ;; - [x] Convert the implementation runner to a generic function.

  ;; TODO:
  ;; - [ ] Make sure that it's easy to check pieces (e.g. init-fn etc) of your
  ;;       trace checking.
  ;; - [ ] It should also be easy to have the generated steps so the user can
  ;;       run it independtly with arrudeia (instead of being constrained
  ;;       to run everything or nothing, let the user run things in pieces).
  ;; - [ ] Show better diff if inconsistency occurs.
  ;; - [ ] Create clean `app` ns so we can create a version with the addition
  ;;       of arrudeia later.
  ;; - [ ] Add timeout.
  ;; - [ ] Find some way to clear all the processes.

  ;; TALK TODO:
  ;; - [ ] Change the model so it creates a counterexample so you can add it
  ;;       as one of your implementation tests.
  ;; - [ ] Create schematic of what happens behind the scenes in Recife.
  ;; - [ ] Create schematic of what happens in the analyzer (lifecycle).
  ;; - [ ] It's not easy, you have to come with your own assertions about the model
  ;;       and then create the mapping to the implementation, it requires some thought.
  ;; - [ ] We can have a bidirectional feedback between model and implementation.

  ())

(def c-trace
  [[0
    {:recife.core/procs
     {:initial-request {:pc :example.partner-2/initial-request},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/handle-request},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {},
      :c3 {:children #{:c1}},
      :c4 {}},
     :id/counter 0,
     :partner/history []}]
   [1
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/handle-request},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{:c4 :c2},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 0,
     :partner/history [],
     :recife/metadata
     {:context
      [:example.partner-2/initial-request {:self :initial-request}]}}]
   [2
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/handle-request},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{:c4},
     :webhook/reqs #{[:c2 0]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 1,
     :partner/history [[:c2 0]],
     :recife/metadata
     {:context [:partner/server {:req :c2, :self :p2}]}}]
   [3
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/handle-request},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{},
     :webhook/reqs #{[:c2 0] [:c4 1]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :recife/metadata
     {:context [:partner/server {:req :c4, :self :p2}]}}]
   [4
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/load-companies},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{},
     :webhook/reqs #{[:c4 1]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :recife/metadata
     {:context [:webhook/handle-request {:req [:c2 0], :self :w1}]}}]
   [5
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/load-companies},
      :w2 {:pc :webhook/load-companies}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :recife/metadata
     {:context [:webhook/handle-request {:req [:c4 1], :self :w2}]}}]
   [6
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}},
      :w2 {:pc :webhook/load-companies}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :recife/metadata
     {:context [:webhook/load-companies {:self :w1}]},
     :global/locked? true}]
   [7
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}},
      :w2 {:pc :webhook/load-companies}},
     :partner/reqs #{:c1},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :recife/metadata
     {:context [:webhook/send-to-partner {:self :w1}]},
     :global/locked? false}]
   [8
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}},
      :w2
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{:c1},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :recife/metadata
     {:context [:webhook/load-companies {:self :w2}]},
     :global/locked? true}]
   [9
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}},
      :w2
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{[:c1 2]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 3,
     :partner/history [[:c2 0] [:c4 1] [:c1 2]],
     :recife/metadata
     {:context [:partner/server {:req :c1, :self :p2}]},
     :global/locked? true}]
   [10
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}},
      :w2
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{:c1},
     :webhook/reqs #{[:c1 2]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 3,
     :partner/history [[:c2 0] [:c4 1] [:c1 2]],
     :recife/metadata
     {:context [:webhook/send-to-partner {:self :w2}]},
     :global/locked? false}]
   [11
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}},
      :w2
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{[:c1 2] [:c1 3]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c1 3]],
     :recife/metadata
     {:context [:partner/server {:req :c1, :self :p2}]},
     :global/locked? false}]])

(def trace
  [[0
    {:recife.core/procs
     {:initial-request {:pc :example.partner-2/initial-request},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/handle-request},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {},
      :c3 {:children #{:c1}},
      :c4 {}},
     :id/counter 0,
     :partner/history []}]
   [1
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/handle-request},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{:c4 :c2},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 0,
     :partner/history [],
     :recife/metadata
     {:context
      [:example.partner-2/initial-request {:self :initial-request}]}}]
   [2
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/handle-request},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{:c4},
     :webhook/reqs #{[:c2 0]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 1,
     :partner/history [[:c2 0]],
     :recife/metadata
     {:context [:partner/server {:req :c2, :self :p1}]}}]
   [3
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/load-companies},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{:c4},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 1,
     :partner/history [[:c2 0]],
     :recife/metadata
     {:context [:webhook/handle-request {:req [:c2 0], :self :w1}]}}]
   [4
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1 {:pc :webhook/load-companies},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{},
     :webhook/reqs #{[:c4 1]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :recife/metadata
     {:context [:partner/server {:req :c4, :self :p2}]}}]
   [5
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2 {:pc :webhook/handle-request}},
     :partner/reqs #{},
     :webhook/reqs #{[:c4 1]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :global/locked? true,
     :recife/metadata
     {:context [:webhook/load-companies {:self :w1}]}}]
   [6
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2 {:pc :webhook/load-companies}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :global/locked? true,
     :recife/metadata
     {:context [:webhook/handle-request {:req [:c4 1], :self :w2}]}}]
   [7
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2 {:pc :webhook/load-companies}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :global/locked? false,
     :recife/metadata
     {:context [:webhook/send-to-partner {:self :w1}]}}]
   [8
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :global/locked? true,
     :recife/metadata
     {:context [:webhook/load-companies {:self :w2}]}}]
   [9
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{:c1},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 2,
     :partner/history [[:c2 0] [:c4 1]],
     :global/locked? false,
     :recife/metadata
     {:context [:webhook/send-to-partner {:self :w2}]}}]
   [10
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{[:c1 2]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 3,
     :partner/history [[:c2 0] [:c4 1] [:c1 2]],
     :global/locked? false,
     :recife/metadata
     {:context [:partner/server {:req :c1, :self :p2}]}}]
   [11
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/load-companies,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 3,
     :partner/history [[:c2 0] [:c4 1] [:c1 2]],
     :global/locked? false,
     :recife/metadata
     {:context [:webhook/handle-request {:req [:c1 2], :self :w2}]}}]
   [12
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}},
      :c4 {:sent? true, :id 1}},
     :id/counter 3,
     :partner/history [[:c2 0] [:c4 1] [:c1 2]],
     :global/locked? true,
     :recife/metadata
     {:context [:webhook/load-companies {:self :w2}]}}]
   [13
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{:c3},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true},
      :c4 {:sent? true, :id 1}},
     :id/counter 3,
     :partner/history [[:c2 0] [:c4 1] [:c1 2]],
     :global/locked? false,
     :recife/metadata
     {:context [:webhook/send-to-partner {:self :w2}]}}]
   [14
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{[:c3 3]},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c3 3]],
     :global/locked? false,
     :recife/metadata
     {:context [:partner/server {:req :c3, :self :p2}]}}]
   [15
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :partner/server},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/load-companies,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true, :id 3},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c3 3]],
     :global/locked? false,
     :recife/metadata
     {:context [:webhook/handle-request {:req [:c3 3], :self :w2}]}}]
   [16
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :recife.core/done},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/load-companies,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true, :id 3},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c3 3]],
     :global/locked? false,
     :recife/metadata
     {:context [:partner/server {:req nil, :self :p1}]}}]
   [17
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :recife.core/done},
      :w1
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}, :sent? true, :id 3},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true, :id 3},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c3 3]],
     :global/locked? true,
     :recife/metadata
     {:context [:webhook/load-companies {:self :w2}]}}]
   [18
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :recife.core/done},
      :w1
      {:pc :recife.core/done,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/send-to-partner,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}, :sent? true, :id 3},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true, :id 3},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c3 3]],
     :global/locked? true,
     :recife/metadata
     {:context [:webhook/handle-request {:req nil, :self :w1}]}}]
   [19
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :partner/server},
      :p1 {:pc :recife.core/done},
      :w1
      {:pc :recife.core/done,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}, :sent? true, :id 3},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true, :id 3},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c3 3]],
     :global/locked? false,
     :recife/metadata
     {:context [:webhook/send-to-partner {:self :w2}]}}]
   [20
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :recife.core/done},
      :p1 {:pc :recife.core/done},
      :w1
      {:pc :recife.core/done,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :webhook/handle-request,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}, :sent? true, :id 3},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true, :id 3},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c3 3]],
     :global/locked? false,
     :recife/metadata
     {:context [:partner/server {:req nil, :self :p2}]}}]
   [21
    {:recife.core/procs
     {:initial-request {:pc :recife.core/done},
      :p2 {:pc :recife.core/done},
      :p1 {:pc :recife.core/done},
      :w1
      {:pc :recife.core/done,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}},
        :c4 {:sent? true}}},
      :w2
      {:pc :recife.core/done,
       :loaded-companies
       {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
        :c2 {:sent? true, :id 0},
        :c3 {:children #{:c1}, :sent? true, :id 3},
        :c4 {:sent? true, :id 1}}}},
     :partner/reqs #{},
     :webhook/reqs #{},
     :example.partner-2/companies
     {:c1 {:children #{:c4 :c2}, :sent? true, :id 2},
      :c2 {:sent? true, :id 0},
      :c3 {:children #{:c1}, :sent? true, :id 3},
      :c4 {:sent? true, :id 1}},
     :id/counter 4,
     :partner/history [[:c2 0] [:c4 1] [:c1 2] [:c3 3]],
     :global/locked? false,
     :recife/metadata
     {:context [:webhook/handle-request {:req nil, :self :w2}]}}]])
