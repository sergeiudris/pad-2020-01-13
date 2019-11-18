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
  {:cmu.filename/summaries "plot_summaries.txt"
   :cmu.filename/metadata "movie.metadata.tsv"
   :cmu.dir/movie-summaries "MovieSummaries/"
   })

(def opts
  {:cmu.dir/shell "/opt/app/"
   :cmu.dir/target "/opt/app/tmp/data/cmu/"})

(defn bash-script-fetch-cmu
  [{:cmu.dir/keys [target]}]
  (format "
  DIR=%s
  mkdir -p $DIR
  cd $DIR
  wget http://www.cs.cmu.edu/~ark/personas/data/MovieSummaries.tar.gz
  tar -xvzf MovieSummaries.tar.gz
  # mv ./MovieSummaries/* ./
  " target))

(defn fetch-cmu
  [{:cmu.dir/keys [shell] :as opts}]
  (sh "bash" "-c" (bash-script-fetch-cmu opts) :dir shell))

#_(fetch-cmu {:shell-dir "/opt/app"
              :target-dir "/opt/app/tmp/data/cmu"})

(defn data-dir
  [{target-dir :cmu.dir/target}]
  (str target-dir (:cmu.dir/movie-summaries -conf)))

#_(data-dir opts)

(comment

  (read-nth-line (str (data-dir opts) (:cmu.filename/summaries -conf)) 1)
  (-> (read-nth-line (str (data-dir opts) (:cmu.filename/metadata -conf)) 2) (string/split #"\t"))
  
  ;
  )

(defn csv-file>>vec!
  [filename & {:keys [separator] :or {separator "\t"}}]
  (with-open [reader (io/reader filename)]
    (->> reader
         (line-seq)
         (map #(string/split % (re-pattern (str separator))))
         (vec))))

#_(def mdata (csv-file>>vec! (str (data-dir opts) (:cmu.filename/metadata -conf))))
#_(-> mdata (first))

(defn csv-vec>>entities
  [columns data]
  (->> data
       (mapv #(reduce-kv
               (fn [a i v]
                 (assoc a (get columns i) v)) {} %))))

(defn read-metadata!
  [{:keys [] :as opts}]
  (->> (str (data-dir opts) (:cmu.filename/metadata -conf))
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
  (->> (str (data-dir opts) (:cmu.filename/metadata -conf))
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


(comment


  (do
    (def mdata (read-metadata! opts))
    (def summs (read-summaries! opts))
    (def data (data>>joined mdata summs))
    (def data-sorted (->> data (sort-by :box-office >)))
    (def data-sorted-map (->> data-sorted (reduce #(assoc %1 (:id-wiki %2) %2) {}))))

  (count data-sorted)
  (->> data-sorted (map :box-office) (take 10))
  (->> data-sorted (take 20) (map #(select-keys % [:id-wiki :name :box-office])))
  (-> data-sorted-map (get "174251") (select-keys [:id-wiki :name :box-office]))

  ;
  )




