(ns pad.ml.mxnet
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
            [org.apache.clojure-mxnet.ndarray-api :as ndapi]
            [org.apache.clojure-mxnet.dtype :as dtype]
            [org.apache.clojure-mxnet.callback :as callback]
            [org.apache.clojure-mxnet.layout :as layout]
            [org.apache.clojure-mxnet.random :as random]
            [org.apache.clojure-mxnet.shape :as shape]
            [org.apache.clojure-mxnet.infer :as infer]
            [org.apache.clojure-mxnet.visualization :as viz]))



(defn cos-sim
  [a b]
  (nd// (nd/dot a b) (nd/* (nd/norm a) (nd/norm b))))



#_(def a (nd/array [1 2] [2]))
#_(def b (nd/array [-1 -2] [2]))
#_(def c (nd/array [10 20] [2]))
#_(nd/->vec (nd// a b))
#_(nd/->vec (cos-sim a b))
#_(nd/->vec (cos-sim a c))

(defn normalize
  [a]
  (ndapi/broadcast-div a (nd/norm a)))

(defn normalize-row
  [v]
  (let [a (nd/array v [(count v)])]
    (as-> a x
      (nd/* x x)
      (nd/sum x)
      (nd/+ x 1E-10)
      (nd/sqrt x)
      (nd/broadcast-div a x))))

#_(nd/norm (nd/array [1 2 3 4] [4]))
#_(normalize (nd/array [1 2 3 4] [4]))
#_(nd/norm (normalize (nd/array [1 2 3 4] [4])))
#_(def a (nd/array [1 2 3 4] [4]))
#_(def b (normalize a))

