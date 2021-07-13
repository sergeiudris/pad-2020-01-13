(ns pad.dataset.arxiv
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [clojure.xml]
            [clojure.data.xml :as xml]
            [cheshire.core :as json]
            [pad.ml.nlp :refer [build-vocab]]
            [pad.prn.core :refer [linst]]
            [pad.coll.core :refer [contained?]]
            [pad.io.core :refer [read-nth-line count-lines]]
            [pad.core :refer [str-float? str>>float resolve-var]]))

(def -conf
  {
   
   })

(def categories ["cs" "econ" "eess" "math" "physics" "q-bio" "q-fin" "stat"])
(def categories ["cs" "physics" "math" "q-bio"])
(def padding-token "</s>")

(def opts
  {:arxiv.dir/shell "/opt/app/"
   :arxiv.dir/target "/opt/app/tmp/data/arxiv/"
   :arxiv.categories categories
   })

(defn bash-script-fetch-arxiv-sample
  [{:arxiv.dir/keys [target]}]
  (format "
  DIR=%s
  mkdir -p $DIR
  cd $DIR

  git clone https://github.com/test.deeplearning.data
  mv test.deeplearning.data/datasets/arxiv-20191101/* ./
  rm -rf test.deeplearning.data
  " target))

(defn fetch-arxiv-sample
  [{:arxiv.dir/keys [shell] :as opts}]
  (sh "bash" "-c" (bash-script-fetch-arxiv-sample opts)  :dir shell))

(defn data-dir
  [{target-dir :arxiv.dir/target}]
  target-dir)

#_(data-dir opts)



(defn axriv-xml-file>>article-vec!
  "Returns a vector of  articles' metadata in xml-edn"
  [path]
  (->> path
       (clojure.xml/parse)
       :content
       (last)
       :content
       (butlast)))

#_(def recrods (axriv-xml-file>>article-vec! (str data-dir "oai2-cs-1000.xml")))
#_(count records)
#_(first records)

(defn arxiv-xml>>data
  [xml]
  {:identifier (-> xml :content (first) :content (first) :content (first))
   :title (-> xml :content (second) :content (first) :content (first) :content (first))
   :setSpec (-> xml :content (first) :content (last) :content (first))
   :description (->> xml :content (second) :content (first) :content
                     (reduce #(when (= (:tag %2) :dc:description) (reduced %2)))
                     :content (first))})

(defn arxiv-xml>>edn!
  "Reads xml, xforms and saves to edn"
  [filename]
  (->> filename
       (axriv-xml-file>>article-vec!)
       (map arxiv-xml>>data)
       (take 1000)))

#_(def cs-data (vec (arxiv-xml>>edn! (str data-dir "oai2-cs-1000.xml"))))
#_(take 5 cs-data)
#_(count (take-while :description cs-data))

(defn xml-file>>edn-file!
  [in-file out-file]
  (as-> nil v
    (arxiv-xml>>edn! in-file)
    (vec v)
    (str v)
    #_(with-out-str (pp/pprint v))
    (spit out-file v)))

#_(xml-file>>edn-file! (str data-dir "oai2-cs-1000.xml")
                       (str data-dir "oai2-cs-1000.edn.txt"))

#_(doseq [c categories]
    (xml-file>>edn-file! (str data-dir "oai2-" c "-1000.xml")
                         (str data-dir "oai2-" c "-1000.edn.txt")))

#_(def data (->> categories
                 (mapcat (fn [c]
                           (->> (str data-dir "oai2-" c "-1000.xml")
                                (arxiv-xml>>edn!)
                                (vec))))
                 (vec)))

#_(count data)
#_(->> data (map :setSpec) (distinct))

(defn categories>>data!
  [{:arxiv/keys [categories] :as opts}]
  (->> categories
       (mapcat (fn [c]
                 (->> (str (data-dir opts) "oai2-" c "-1000.xml")
                      (arxiv-xml>>edn!)
                      (vec))))))

#_(def data (categories>>data! categories))



(defn clean-str [s]
  (-> s
      (string/replace #"^A-Za-z0-9(),!?'`]" " ")

      (string/replace #"\." " . ")
      (string/replace #"\"" "")

      (string/replace #"'s" " 's")
      (string/replace #"'ve" " 've")
      (string/replace #"n't" " n't")
      (string/replace #"'re" " 're")
      (string/replace #"'d" " 'd")
      (string/replace #"'ll" " 'll")
      (string/replace #"," " , ")
      (string/replace #"!" " ! ")
      (string/replace #"\(" " ( ")
      (string/replace #"\)" " ) ")
      (string/replace #"\?" " ? ")
      (string/replace #" {2,}" " ")
      (string/trim)))

#_(string/replace "$\\sigma>0$" #"\$" "")

(defn data>>labels
  "Maps article metadata into {label-name normalized-value}"
  [data]
  (let [categories (->> data (map :setSpec) (distinct) (vec))
        size (dec (count categories))]
    (->> categories (reduce-kv #(assoc %1 %3 (/ (float %2) size)) {}))))

(defn  data>>labeled
  "Adds :label to data "
  [data]
  (let [labels (data>>labels data)]
    (mapv #(->> (get labels (:setSpec %))
                (assoc % :label)) data)))

#_(def data-labeled (data>>labeled data))
#_(data>>labels data)
#_(count data-labeled)
#_(nth data-labeled 7000)

(defn text>>tokens
  [text]
  (-> text
      (clean-str)
      (string/split #"\s+")))

(defn data>>tokened
  "Adds :tokens to datapoints"
  [data]
  (mapv #(assoc % :tokens (->> % :description (text>>tokens))) data))

(defn tokened>>limited
  "Limits the length of token (sentence)"
  [data & {:keys [tokens-limit]}]
  (mapv (fn [v]
          (update v :tokens #(->> % (take (or tokens-limit ##Inf)) (vec)))) data))

(defn tokens>>padded
  [tokens padding-token max-seq-length]
  (let [diff (- max-seq-length (count tokens))]
    (into tokens  (repeat diff padding-token))))

(defn data>>padded
  [data]
  (let [max-seq-length (->> data (mapv #(count (:tokens %))) (apply max))]
    (mapv #(assoc % :tokens
                  (-> % :tokens (tokens>>padded padding-token max-seq-length)))
          data)))

#_(def data-tokened (data>>tokened data-labeled))
#_(nth data-tokened 1000)

#_(def data-padded (data>>padded data-tokened))
#_(nth data-padded 1000)


#_(def vocab (build-vocab (map :tokens data-padded)))
#_(count vocab) ; 50953
#_(nth (seq vocab) 1)



(defn data>>embedded
  "Adds :embedded [[..]]"
  [data embeddings]
  (mapv
   (fn [v]
     (assoc v :embedded (mapv #(embeddings %) (:tokens v)))) data))

#_(def data-embedded (data>>embedded data-padded vocab-embeddings))
#_(-> data-embedded (nth 5000) :embedded (flatten) (count)) ; 26700 (* 534 50)
#_(->> data-embedded (map #(-> %  :embedded (flatten) (count))) (reduce + 0)); 213600000
; = 213600000 (* 8000 26700)
#_(-> data-embedded   (first) (dissoc :embedded :tokens))

#_(def data-shuffled (shuffle data))
#_(type data-shuffled)

#_(do
    (def data (categories>>data! categories))
    (def glove (read-glove! (glove-path embedding-size)))
    (def data-labeled (data>>labeled data))
    (def data-tokened (data>>tokened data-labeled))
    (def data-limited (tokened>>limited data-tokened :tokens-limit 128))
    (def data-padded (data>>padded data-tokened))
    (def vocab (-> (build-vocab (map :tokens data-padded)) :indexes))
    (def vocab-embeddings (build-vocab-embeddings vocab glove embedding-size))
    (def data-embedded (data>>embedded data-padded vocab-embeddings))
    (def data-shuffled (shuffle data-embedded)))

#_(count data-shuffled)
#_(->> data-shuffled (take 30) (map :setSpec))
#_(->> data-shuffled (map :setSpec) (distinct))
#_(->> data-shuffled (first) :embedded (count))
#_(->> data-tokened (first) :tokens (count))
#_(->> data-limited (first) :tokens (count))
#_(->> data-shuffled (first) :tokens (count))