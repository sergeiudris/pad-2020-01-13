(ns pad.dataset.glove
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [clojure.xml]
            [clojure.data.xml :as xml]
            [pad.prn.core :refer [linst]]
            [pad.coll.core :refer [contained?]]
            [pad.io.core :refer [read-nth-line count-lines]]
            [pad.core :refer [str-float? str>>float resolve-var]]))

(def -conf
  {
   :glove.filename/glove-fmt "glove.6B.%dd.txt"
   })

(def opts
  {:glove.dir/shell "/opt/app/"
   :glove.dir/target "/opt/app/tmp/data/glove/"
   :glove/embedding-size 50
   })

(defn bash-script-fetch-glove
  [{:glove.dir/keys [target]}]
  (format "
  DIR=%s
  mkdir -p $DIR
  cd $DIR

  wget http://nlp.stanford.edu/data/glove.6B.zip
  
  unzip *.zip
  " target))

(defn fetch-glove
  [{:glove.dir/keys [shell] :as opts}]
  (sh "bash" "-c" (bash-script-fetch-glove opts) :dir shell))

(defn data-dir
  [{target-dir :glove.dir/target}]
  target-dir)

#_(data-dir opts)

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
    {:vec (vec xs)
     :idx-to-token (mapv first xs)
     :token-to-embedding (into {} xs)
     :token-to-idx (->> xs (map-indexed #(vector (first %2) %1)) (into {}))}))

(defn glove-filepath
  [{embedding-size :glove/embedding-size
    :as opts}]
  (format (str (data-dir opts) (format (:glove.filename/glove-fmt -conf)  embedding-size))))

(comment

  (nd/concatenate [(nd/array [1 2] [2]) (nd/array [1 2] [2])])
  (def v (get glove-embeddings "matrix"))

  (do
    (def glove (-> (glove-filepath 50) (read-glove!)))
    (def glove-vec (:vec glove))
    (def glove-to-embedding (:token-to-embedding glove))
    (def glove-to-token (:idx-to-token glove))
    (def glove-to-idx (:token-to-idx glove))
    )

  (first glove-vec)
  (get glove-to-embedding "the")
  (get glove-to-token 0)
  (get glove-to-idx "the")

  ;
  )