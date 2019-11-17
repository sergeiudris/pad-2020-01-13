(ns pad.dataset.cmu
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
  {:filename/summaries "plot_summaries.txt"
   :filename/metadata "movie.metadata.tsv"
   :dir/movie-summaries "MovieSummaries/"
   })

(def opts
  {:dir/shell "/opt/app/"
   :dir/target "/opt/app/tmp/data/cmu/"})

(def script-fetch-cmu
  "
  DIR=%s
  mkdir -p $DIR
  cd $DIR
  wget http://www.cs.cmu.edu/~ark/personas/data/MovieSummaries.tar.gz
  tar -xvzf MovieSummaries.tar.gz
  # mv ./MovieSummaries/* ./
  ")

(defn fetch-cmu
  [{:dir/keys [shell target]}]
  (let [script (format script-fetch-cmu target)]
    (sh "bash" "-c" script :dir shell)))

#_(fetch-cmu {:shell-dir "/opt/app"
              :target-dir "/opt/app/tmp/data/cmu"})

(defn data-dir
  [{target-dir :dir/target}]
  (str target-dir (:dir/movie-summaries -conf)))

#_(data-dir opts)

(comment

  (read-nth-line (str (data-dir opts) (:filename/summaries -conf)) 1)
  (-> (read-nth-line (str (data-dir opts) (:filename/metadata -conf)) 2) (string/split #"\t"))
  
  ;
  )

(defn csv-file>>vec!
  [filename & {:keys [separator] :or {separator "\t"}}]
  (with-open [reader (io/reader filename)]
    (->> reader
         (line-seq)
         (map #(string/split % (re-pattern (str separator))))
         (vec))))

#_(def mdata (csv-file>>vec! (str (data-dir opts) (:filename/metadata -conf))))
#_(-> mdata (first))

(defn csv-vec>>entities
  [columns data]
  (->> data
       (mapv #(reduce-kv
               (fn [a i v]
                 (assoc a (get columns i) v)) {} %))))

(defn read-metadata!
  [{:keys [] :as opts}]
  (->> (str (data-dir opts) (:filename/metadata -conf))
       (csv-file>>vec!)
       (csv-vec>>entities [:id-wiki :id-freebase :name
                           :release-date :box-office
                           :runtime :languages
                           :countries :genres])
       (mapv (fn [v]
               (update v :box-office #(if-not (empty? %) (Float/parseFloat %) 0))))))

#_(def mdata (read-metadata! opts))
#_(first mdata)

(defn read-summaries!
  [{:keys [] :as opts}]
  (->> (str (data-dir opts) (:filename/metadata -conf))
       (csv-file>>vec!)
       (csv-vec>>entities [:id-wiki :summary])
       (reduce #(assoc %1 (:id-wiki %2) %2) {})))

#_(def summs (read-summaries! opts))
#_(first summs)
#_(count summs)

(defn data>>joined
  [mdata summs]
  (->> mdata
       (mapv (fn [v]
               (assoc v :summary (-> summs (get (:id-wiki v)) :summary))))
       (filterv :summary)))

#_(def data (data>>joined mdata summs))
#_(nth data 3)
#_(count data)
#_(->> data (filter :summary) (count))

#_(do
    (def mdata (read-metadata!))
    (def summs (read-summaries!))
    (def data (data>>joined mdata summs))
    (def bert-vocab (bert/read-vocab-json! (str bert-dir bert-base-vocab-filename)))
    (def data-tokened (bert/data>>tokened data #(:summary %)))
    (def data-filtered (->> data-tokened (filterv #(<= (-> % :tokens (count)) 254))))
    ; (def data-padded (bert/data>>padded data-filtered bert-vocab {:seq-length 512}))
    (def data-sorted (->> data-filtered (sort-by :box-office >)))
    (def seq-length 512)
    (def data-sorted-map (->> data-sorted (reduce #(assoc %1 (:id-wiki %2) %2) {}))))

#_(->> (get bert-vocab "idx_to_token") (count)) ; 30522
#_(->> (get bert-vocab "token_to_idx") (count)) ; 30522
#_(-> (get bert-vocab "token_to_idx") (get "[SEP]"))

#_(count data-sorted)
#_(->> data-tokened (map #(count (:tokens %))) (apply max))
#_(->> data-tokened (filter #(< (-> % :tokens (count)) 128)) (count))
#_(->> data-tokened (map :box-office) (take 10))
#_(->> data-tokened
       (filter #(< (-> % :tokens (count)) 128))
       (sort-by :box-office >)
       (take 20)
       (map #(select-keys % [:name :box-office])))
#_(-> data-sorted (first) :tokens (count))
#_(->> data-sorted (take 20) (map #(select-keys % [:id-wiki :name :box-office])))
#_(-> data-sorted-map (get "161190") (select-keys [:id-wiki :name :box-office]))

