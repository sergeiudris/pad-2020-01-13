(ns pad.dataset.house-prices
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [clojure.xml]
            [clojure.data.xml :as xml]
            [clojure.data.csv :refer [read-csv]]
            [pad.prn.core :refer [linst]]
            [pad.coll.core :refer [contained?]]
            [pad.io.core :refer [read-nth-line count-lines]]
            [pad.core :refer [str-float? str>>float resolve-var]]
            [pad.math.core :refer [vec-standard-deviation-2
                                   scalar-subtract elwise-divide
                                   vec-mean scalar-divide
                                   mk-one-hot-vec std]]))

(def -conf
  {:house-prices.filename/train "train.csv"
   :house-prices.filename/test "test.csv"
   })

(def opts
  {:house-prices.dir/shell "/opt/app/"
   :house-prices.dir/target "/opt/app/tmp/data/house-prices/"})

(defn bash-script-fetch-dataset
  [{:house-prices.dir/keys [target]}]
  (format "
  DIR=%s
  mkdir -p $DIR
  cd $DIR

  # wget https://www.kaggle.com/c/house-prices-advanced-regression-techniques/download/train.csv
  # wget https://www.kaggle.com/c/house-prices-advanced-regression-techniques/download/test.csv

  wget https://s3.us-east-2.amazonaws.com/tech.public.data/house-prices-advanced-regression-techniques.zip
  unzip house-prices-advanced-regression-techniques.zip
  " target))

(defn fetch-dataset
  [{:house-prices.dir/keys [shell] :as opts}]
  (sh "bash" "-c" (bash-script-fetch-dataset opts) :dir shell))

#_(:exit (sh "bash" "-c" "sudo chmod -R 777 tmp/" :dir "/opt/app"))

(defn data-dir
  [{target-dir :house-prices.dir/target}]
  target-dir)

#_(data-dir opts)
#_(.exists (io/file (str (data-dir opts) (:house-prices.filename/train -conf) )))

(defn read-column-mdata
  [{:keys [nulls] :or {nulls []}}]
  (letfn [(val>>column-type [v]
            (cond
              (str-float? v) :float
              :else :string))
          (column-type [rows column-idx]
            (->>
             rows
             (map #(nth % column-idx))
             (take-while #(not (contained? % nulls)))
             #_(take-last 1)
             (last)
             (val>>column-type)))
          (column [a k v rows]
            (let [dtype (column-type rows k)]
              (assoc a k (merge
                          {:idx k
                           :val v
                           :dtype dtype}
                          (when (= dtype :string)
                            {:distinct (distinct (mapv #(nth % k) rows))})))))]
    (with-open [reader-train (io/reader (str (data-dir opts) (:house-prices.filename/train -conf)))
                reader-test (io/reader (str (data-dir opts) (:house-prices.filename/test -conf)))]
      (let [data-train (read-csv reader-train)
            data-test (read-csv reader-test)
            rows-train (map #(-> % (rest) (butlast)) data-train)
            rows-test (map #(-> % (rest)) data-test)
            attrs (-> (first data-train) (rest) (butlast))
            rows (concat rows-train rows-test)]
        (reduce-kv (fn [a k v]
                     (column a k v rows))
                   (sorted-map) (vec attrs))))))

#_(read-column-mdata {:nulls ["NA"]})


#_(std [1 2 -1 0 4])
#_(std [10, 12, 23, 23, 16, 23, 21, 16]) ;4.898979485566356 4.8989794855664
#_(standarddev [1 2 -1 0 4])

(defn standardize
  [v]
  (->> (scalar-subtract  (vec-mean v) v)
       (scalar-divide (std v))))

#_(def a-row (with-open [reader (io/reader (str (data-dir opts) (:filename/train -conf)))]
               (let [data (read-csv reader)
                     rows (rest data)]
                 (->> (nth rows 16)
                      (rest)
                      (butlast)
                      (map str>>float)
                      (filter number?)
                      (vec))))) ; 11241

#_(standardize [1 0 0 0])
#_(vec-mean [1 0 0 0])
#_(standardize [1 0 0 0 1/4])

#_(vec-mean [1 2 3])
#_(standardize [1 2 3 2])
#_(standardize [1000000 2 0 1])


#_(standardize a-row)
#_(std a-row) ; 1920.3065877667573 1920.3065877668 
#_(vec-mean a-row) ;702.4571428571429 702.45714285714
#_(scalar-subtract  (vec-mean a-row) a-row)

#_(standardize-2 a-row)

#_(vec-standard-deviation-2 a-row)
#_(standarddev a-row)
#_(vec-mean a-row)
#_(mean a-row)
#_(scalar-subtract  (vec-mean a-row) a-row)


(defn csv>>data
  [{:keys [filename nulls row>>row-vals row>>score data>>rows]
    :or {data>>rows (fn [data] (rest data))}}]
  (letfn [(val-null-float? [v dtype]
            (and  (= dtype :float) (contained? v nulls)))
          (val-string? [v dtype]
            (= dtype :string))
          (val-float? [v dtype]
            (= dtype :float))
          (col-idx-float? [idx colsm]
            (= (get-in colsm [idx :dtype]) :float))
          (row>>row-nums [row colsm]
            (map-indexed (fn [i x]
                           (if  (col-idx-float? i colsm)
                             (str>>float x)
                             x)) row))
          (string-field>>val [colm v]
            (->> (:distinct colm)
                 (keep-indexed (fn [i x]
                                 (if (= x v);  
                                   (mk-one-hot-vec (count (:distinct colm)) i)
                                   nil)))
                 (first)))
          (float-field>>val [colm v row row-mean])
          (row>>mean [row]
            (->> row
                 (keep-indexed (fn [i v]
                                 (if (number? v) v nil)))
                 (vec-mean)))
          (row>>float-features [row]
            (->> row
                 (filter number?)
                 (standardize)))
          (row>>float-null-features [row]
            (->> row
                 (filter string?)
                 (map (constantly 0.0))))
          (row>>string-features [row]
            (filter coll? row))
          (attr>>val [idx v row rows colsm row-mean]
            (let [colm (get colsm idx)
                  dtype (:dtype colm)]
              (cond
                (val-string? v dtype) (string-field>>val colm v)
                (val-null-float? v dtype) v
                (val-float? v dtype) v
                :else (throw (Exception. "uknown/missing column :dtype")))))]
    (with-open [reader (io/reader filename)]
      (let [data (read-csv reader)
            rows (data>>rows data)
            colsm (read-column-mdata {:nulls nulls})]
        (mapv (fn [row]
                (let [row-vals (row>>row-vals row)
                      row-nums (row>>row-nums row-vals colsm)
                      row-mean (row>>mean row-nums)
                      row-hots (map-indexed
                                (fn [idx v]
                                  (attr>>val idx v row-vals rows colsm row-mean))
                                row-nums)
                      float-features
                      #_(->> (range 0 36) (mapv (fn [_] (rand))))
                      (row>>float-features row-hots)
                      float-null-features (row>>float-null-features row-hots)

                      string-features (row>>string-features row-hots)]
                  {:id (first row)
                   :features (-> (concat  float-features float-null-features string-features) (flatten) (vec))
                   :score (row>>score row)})) rows)))))

(defn csv-file>>edn-file!
  [opts]
  (->>
   (csv>>data opts)
   (str)
   (spit (:filename-out opts))))

(defn edn-file>>data!
  [filename]
  (-> filename
      (slurp)
      (read-string)))

#_(csv-file>>edn-file! {:filename (str (data-dir opts) (:house-prices.filename/train -conf))
                        :filename-out (str (data-dir opts) "train.csv.txt")
                        :nulls ["NA"]
                        :row>>row-vals (fn [row]
                                         (-> row (rest) (butlast)))
                        :row>>score (fn [row]
                                      [(str>>float (last row))])})

#_(csv-file>>edn-file! {:filename (str (data-dir opts) (:house-prices.filename/test -conf))
                        :filename-out (str (data-dir opts) "test.csv.txt")
                        :nulls ["NA"]
                        :row>>row-vals (fn [row]
                                         (-> row (rest)))
                        :row>>score (fn [row]
                                      [100000])})

#_(-> (slurp (str (data-dir opts) "test.csv.txt")) (read-string) (count))
#_(-> (slurp (str (data-dir opts) "train.csv.txt")) (read-string) (count))
#_(-> (slurp (str (data-dir opts) "train.csv.txt")) (read-string) (first) :features (count))
#_(-> (slurp (str (data-dir opts) "test.csv.txt")) (read-string)  (first) :features (count))

#_(read-nth-line (str (data-dir opts) "train.csv") 704)
#_(read-nth-line (str (data-dir opts) "test.csv") 1)