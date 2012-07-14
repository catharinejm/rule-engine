(ns rule-engine.core
  (:use [datomic.api :only (q db) :as d]))

(def uri "datomic:mem://rule-engine")
(d/create-database uri)
(def conn (d/connect uri))

(def schema-tx
  '[{:db/id #db/id [:db.part/db]
     :db/ident :rule/name
     :db/valueType :db.type/string
     :db/cardinality :db.cardinality/one
     :db/doc "A rule's name"
     :db.install/_attribute :db.part/db}

    {:db/id #db/id [:db.part/db]
     :db/ident :rule/transitions
     :db/valueType :db.type/fn
     :db/cardinality :db.cardinality/many
     :db/doc "A rule's transitions"
     :db.install/_attribute :db.part/db}

    {:db/id #db/id [:db.part/db]
     :db/ident :patient/age
     :db/valueType :db.type/long
     :db/cardinality :db.cardinality/one
     :db/doc "A patient's age"
     :db.install/_attribute :db.part/db}

    {:db/id #db/id [:db.part/db]
     :db/ident :patient/name
     :db/valueType :db.type/string
     :db/cardinality :db.cardinality/one
     :db/doc "A patient's name"
     :db.install/_attribute :db.part/db}

    {:db/id #db/id [:db.part/db]
     :db/ident :re/type
     :db/valueType :db.type/ref
     :db/cardinality :db.cardinality/one
     :db/doc "The type of a rule-engine entity"
     :db.install/_attribute :db.part/db}

   [:db/add #db/id [:db.part/user] :db/ident :re.types/rule]
   [:db/add #db/id [:db.part/user] :db/ident :re.types/patient]
   [:db/add #db/id [:db.part/user] :db/ident :re.types/transition]])

@(d/transact conn schema-tx)
(comment)

(def objects-tx
  '[{:db/id #db/id [:db.part/user -1]
     :re/type :re.types/patient
     :patient/name "Bob"
     :patient/age 45}

    {:db/id #db/id [:db.part/user -2]
     :re/type :re.types/rule
     :rule/name "Root"}

    {:db/id #db/id [:db.part/user -3]
     :re/type :re.types/rule
     :rule/name "Is old"}

    {:db/id #db/id [:db.part/user -4]
     :re/type :re.types/rule
     :rule/name "Is young"}])

(defn patient-age []
  (ffirst (q '[:find ?v :where [?e :patient/age ?v]] (db conn))))

(defn rule-with-name [name]
  (q `[:find ?e :where [?e :rule/name ~name]]))

(def next-step (d/function `{:lang :clojure
                             :params [db step]
                             :code (if (> (~patient-age) 42)
                                     (~rule-with-name "Is old")
                                     (~rule-with-name "Is young"))}))
@(d/transact conn (conj objects-tx
                        {:db/id #db/id [:db.part/user -5] :re/type :re.types/transition :db/fn next-step}))