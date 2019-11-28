(ns pad.core
  (:require [pad.comm]))

(def resolve-var pad.comm/resolve-var)

(defn java-info
  []
  [(System/getProperty "java.version")
   (System/getProperty "java.vm.name")
   (System/getProperty "java.vm.version")])

(defn memory-info
  []
  {:max (-> (Runtime/getRuntime) (.maxMemory) (/ 1000000) (int)  (str "mb"))
   :total (-> (Runtime/getRuntime) (.totalMemory) (/ 1000000) (int)  (str "mb"))
   :free (-> (Runtime/getRuntime) (.freeMemory) (/ 1000000) (int)  (str "mb"))})

(comment
  
  (java-info)

  (System/getProperties )
  (keys (System/getProperties))
  (System/getProperty "java.vm.name")
  (System/getProperty "java.vm.version")
  (System/getProperty "java.version")

  (clojure-version)

  (.. Runtime getRuntime freeMemory)

  (-> (Runtime/getRuntime) (.maxMemory) (/ 1000000) (int)  (str "mb"))
  (-> (Runtime/getRuntime) (.freeMemory) (/ 1000000) (int)  (str "mb"))
  (-> (Runtime/getRuntime) (.totalMemory) (/ 1000000) (int)  (str "mb"))

  (read-string "[:key some-text]")
  (-> (read-string "[:key some-text]") (second) (type))

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

(defn safe-deref-future
  [fu]
  (when (and (future-done? fu)
             (not (future-cancelled? fu)))
    (deref fu)))

(comment

  (def fu (future
            (Thread/sleep 1000)
            (prn "hello")
            (Thread/sleep 10000)
            123))
  (future-cancel fu)

  (if (realized? fu) (deref))

  (future-cancelled? fu)
  (safe-deref-future fu)
  

  ;
  )