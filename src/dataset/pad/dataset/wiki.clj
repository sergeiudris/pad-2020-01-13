(ns pad.dataset.wiki
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
  {:wiki.filename/wiki-sample "enwiki-20191101-pages-articles1.xml-p10p30302"} ; 19817 articles 64417 categories
  )

(def opts
  {:wiki.dir/shell "/opt/app/"
   :wiki.dir/target "/opt/app/tmp/data/wiki-sample/"})

(defn bash-script-fetch-wiki-file
  [{:wiki.dir/keys [target]}]
  (format "
  # https://ftp.acc.umu.se/mirror/wikimedia.org/dumps/enwiki/20191101/

  DIR=%s
  mkdir -p $DIR
  cd $DIR

  FILE=%s

  wget https://ftp.acc.umu.se/mirror/wikimedia.org/dumps/enwiki/20191101/$FILE
  bzip2 -d $FILE
  " target (:wiki.filename/wiki-sample -conf)))

(defn fetch-wiki-sample
  [{:wiki.dir/keys [shell target] :as opts}]
  (sh "bash" "-c" (bash-script-fetch-wiki-file opts) :dir shell))

#_(fetch-wiki-sample {:shell-dir "/opt/app"
                      :target-dir "/opt/app/tmp/data/cmu"})

(defn data-dir
  [{target-dir :wiki.dir/target}]
  target-dir)

(defn sample-filename
  [opts]
  (str (data-dir opts) (:wiki.filename/wiki-sample -conf)))

#_(data-dir opts)
#_(sample-filename opts)


(defn shorten-text
  [v]
  (update
   v :content
   (partial
    map
    (fn [x]
      (if (= (:tag x) :revision)
        (update
         x :content
         (partial
          map
          (fn [y]
            (if (= (:tag y) :text)
              (let [s (-> y :content (first) (str))]
                (assoc
                 y :content (list (subs s 0 (min 500 (count s))))))
              y))))
        x)))))

(comment

  ; https://stackoverflow.com/questions/47832579/clojure-reducing-large-lazy-collection-eats-up-memory

  (def data-raw (slurp (sample-filename opts)))
  (def data-xml (xml/parse-str data-raw))

  (with-open [input (java.io.FileInputStream. (sample-filename opts))]
    (->> input
         (xml/parse)
         (type)))

  (def input-stream (java.io.FileInputStream. (sample-filename opts)))
  (def data-xml (xml/parse input-stream))

  (->> data-xml :content (count))

  (def data-sample-xml (->> data-xml
                            :content
                            (rest)
                            (filter #(->> (:content %) (map :tag) (into #{}) :redirect (not)))
                            #_(take 1)
                            #_(mapv shorten-text)))

  ; uses ~2G of mem
  ; data-sample-xml is a global var, holding head of the seq 
  (count data-sample-xml)

  ; uses ~300mb
  ; no refs are used
  (->> (java.io.FileInputStream. (sample-filename opts))
       (xml/parse)
       :content
       (rest)
       (filter #(->> (:content %) (map :tag) (into #{}) :redirect (not)))
       (count))

  ; (def data-sample-edn (read-string (str data-sample-xml)))

  (def data (->> data-sample-xml
                 (map (fn [x]
                        {:title (->> x :content (filter #(= (:tag %) :title))
                                     (first) :content (first) (str))
                         :id (->> x :content (filter #(= (:tag %) :id))
                                  (first) :content (first) (str))
                         :text (->> x :content (filter #(= (:tag %) :revision))
                                    (first) :content (filter #(= (:tag %) :text))
                                    (first) :content (first) (str))}))
                 (map (fn [x]
                        (assoc x :categories
                               (->> (:text x)
                                    (re-seq #"\[\[Category:(.+)\]\]")
                                    (mapv last)))))))

  ; (map #(select-keys % [:title :categories]) data)

  (->> data (mapcat :categories) (distinct) (count))

  (def text (-> data (first) :text))


  (def cats "\n \n [[Category:Anarchism|]] 
    \n [[Category:Anti-capitalism]] 
    \n [[Category:Anti-fascism]] 
    \n [[Category:Far-left politics]] 
    \n [[Category:Libertarian socialism]] 
    \n [[Category:Political culture]] 
    \n [[Category:Political ideologies]] 
    \n [[Category:Social theories]]")

  (re-seq #"\[\[Category:(.+)\]\]" text)

  ;
  )