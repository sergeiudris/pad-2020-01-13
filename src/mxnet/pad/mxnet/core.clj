(ns pad.mxnet.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]

            [org.apache.clojure-mxnet.io :as mx-io]
            [org.apache.clojure-mxnet.context :as context]
            [org.apache.clojure-mxnet.module :as m]
            [org.apache.clojure-mxnet.symbol :as sym]
            [org.apache.clojure-mxnet.kvstore :as kvstore]
            [org.apache.clojure-mxnet.kvstore-server :as kvstore-server]
            [org.apache.clojure-mxnet.eval-metric :as eval-metric]
            [org.apache.clojure-mxnet.optimizer :as optimizer]
            [org.apache.clojure-mxnet.lr-scheduler :as lr-scheduler]
            [org.apache.clojure-mxnet.initializer :as initializer]
            [org.apache.clojure-mxnet.resource-scope :as resource-scope]
            [org.apache.clojure-mxnet.ndarray :as nd]
            [org.apache.clojure-mxnet.dtype :as dtype]
            [org.apache.clojure-mxnet.callback :as callback]
            [org.apache.clojure-mxnet.layout :as layout]
            [org.apache.clojure-mxnet.random :as random]
            [org.apache.clojure-mxnet.shape :as shape]
            [org.apache.clojure-mxnet.infer :as infer]
            [org.apache.clojure-mxnet.visualization :as viz]))

(defn lines>>word-embeddings
  "maps lines into  [[word embeddings]..]"
  [lines]
  (for [^String line lines
        :let [fields (.split line " ")]]
    [(first fields)
     (mapv #(Float/parseFloat %) (rest fields))]))

(defn read-glove!
  "Reads glove file into {word embeddings}"
  [path]
  (prn "-- reading glove from " path)
  (let [xs (->> (io/reader path)
                (line-seq)
                (lines>>word-embeddings))]
    {:idx-to-token (mapv first xs)
     :token-to-embedding (into {} xs)}))

(defn glove-path
  [glove-dir embedding-size]
  (format (str glove-dir "glove.6B.%dd.txt") embedding-size))

(defn cos-sim
  [a b]
  (nd// (nd/dot a b) (nd/* (nd/norm a) (nd/norm b))))

#_(def a (nd/array [1 2] [2]))
#_(def b (nd/array [-1 -2] [2]))
#_(def c (nd/array [10 20] [2]))
#_(nd/->vec (nd// a b))
#_(nd/->vec (cos-sim a b))
#_(nd/->vec (cos-sim a c))


