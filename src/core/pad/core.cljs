(ns pad.core
  (:require [pad.comm]))

; TODO iter?
; export cljc deps
(def resolve-var pad.comm.resolve-var)

(defn str>>float
  ([s]
   (str>>float s s))
  ([s alt]
   (try (Float/parseFloat s)
        (catch Exception e alt))))

(defn str-float?
  [s]
  (number? (str>>float s)))