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
   :filename/glove-fmt "glove.6B.%dd.txt"
   })

(def opts
  {:dir/shell "/opt/app/"
   :dir/target "/opt/app/tmp/data/glove/"
   :embedding-size 50
   })

(defn script-fetch-glove
  [{:dir/keys [target]}]
  (format "
  DIR=%s
  mkdir -p $DIR
  cd $DIR

  wget http://nlp.stanford.edu/data/glove.6B.zip
  
  unzip *.zip
  " target))

(defn fetch-glove
  [{:dir/keys [shell] :as opts}]
  (let [script (format (script-fetch-glove opts))]
    (sh "bash" "-c" script :dir shell)))

(defn data-dir
  [{target-dir :dir/target}]
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

(defn glove-filename
  [{:keys [embedding-size] :as opts}]
  (format (str (data-dir opts) (format (:filename/glove-fmt -conf)  embedding-size))))