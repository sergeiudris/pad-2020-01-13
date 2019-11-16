(ns pad.core
  (:require [pad.comm]))

(def resolve-var pad.comm/resolve-var)

(defn java-version
  []
  (System/getProperty "java.vm.version"))

(comment

  (System/getProperty "java.vm.version")
  (System/getProperty "java.version")
  (System/getProperty "java.specification.version")
  (clojure-version)

  (.. Runtime getRuntime freeMemory)

  (-> (Runtime/getRuntime) (.maxMemory) (/ 1000000) (int)  (str "mb"))
  (-> (Runtime/getRuntime) (.freeMemory) (/ 1000000) (int)  (str "mb"))
  (-> (Runtime/getRuntime) (.totalMemory) (/ 1000000) (int)  (str "mb"))

  ;
  )

(defn str>>float
  ([s]
   (str>>float s s))
  ([s alt]
   (try (Float/parseFloat s)
        (catch Exception e alt))))

(defn str-float?
  [s]
  (number? (str>>float s)))