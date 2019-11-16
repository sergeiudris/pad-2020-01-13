(ns pad.prn.core
  (:require [clojure.repl :refer :all]
            [clojure.reflect :refer :all]
            [clojure.pprint :as pp]
            [clojure.java.javadoc :refer [javadoc]]
            ; [puget.printer :as pug]
            [clojure.string :as cstr]
            ;
            ))



(defn linst-methods
  [v]
  (->> v
       reflect
       :members
       (filter #(contains? (:flags %) :public))
       (filter #(or (instance? clojure.reflect.Method %)
                    (instance? clojure.reflect.Constructor %)))
       (sort-by :name)
       (map #(select-keys % [:name :return-type :parameter-types]))
       pp/print-table))

(defn linst-fields
  [v]
  (->> v
       reflect
       :members
       (filter #(contains? (:flags %) :public))
       (filter #(not (or (instance? clojure.reflect.Method %)
                         (instance? clojure.reflect.Constructor %))))
       (sort-by :name)
       (map #(select-keys % [:name :return-type :parameter-types]))
       pp/print-table))

(defn linst
  [v]
  (linst-fields v)
  (linst-methods v))

(defn javadoc-print-url
  "Opens a browser window displaying the javadoc for the argument.
  Tries *local-javadocs* first, then *remote-javadocs*."
  {:added "1.2"}
  [class-or-object]
  (let [^Class c (if (instance? Class class-or-object)
                   class-or-object
                   (class class-or-object))]
    (if-let [url (#'clojure.java.javadoc/javadoc-url (.getName c))]
    ;   (browse-url url)
      url
      (println "Could not find Javadoc for" c))))

(comment

  (source javadoc)
  (source clojure.java.javadoc/javadoc-url)


  (apropos "javadoc-url")

  (javadoc-print-url Runtime)
  (javadoc-print-url String)

;;;
  )

(defn sytem-prn
  "print using System.out.println"
  [msg]
  (.println (System/out) msg))