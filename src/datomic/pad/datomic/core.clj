(ns pad.datomic.core
  (:require [clojure.repl :refer :all]
            [datomic.api :as d]
            [clojure.pprint :as pp]))


(defn get-paginted-entity
  "Returns entities and total count given limit,offset and attribute keword"
  [{:keys [db attribute limit offset]
    :or   {limit  10
           offset 0}}]
  {:entities (->>
              (d/q '{:find  [?e (count ?e)]
                     :in    [$ ?attribute]
                     :where [[?e ?attribute]]}
                   db attribute)
              (drop offset)
              (take limit)
              (map  #(identity (d/pull db '[*] (first %))))
              (into []))
   :count    (d/q '{:find  [(count ?e) .]
                    :in    [$ ?attribute]
                    :where [[?e ?attribute]]}
                  db attribute)})


(defn count-total
  [db attribute]
  (d/q '{:find  [(count ?e) .]
         :in    [$ ?attribute]
         :where [[?e ?attribute]]}
       db attribute))

(defn q-idents
  "find the idents of all schema elements in the system"
  [db]
  (sort (d/q '[:find [?ident ...]
               :where [_ :db/ident ?ident]]
             db)))


(defn q-attrs
  "find just the attributes"
  [db]
  (sort (d/q '[:find [?ident ...]
               :where
               [?e :db/ident ?ident]
               [_ :db.install/attribute ?e]]
             db)))

(defn excise-eids
  [conn eids]
  (do @(d/transact conn
                   (mapv (fn [e]
                           {:db/excise e}) eids))))