(ns pad.dataset.bert
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [clojure.xml]
            [clojure.data.xml :as xml]
            [cheshire.core :as json]
            [pad.prn.core :refer [linst]]
            [pad.coll.core :refer [contained?]]
            [pad.io.core :refer [read-nth-line count-lines]]
            [pad.core :refer [str-float? str>>float resolve-var]]))

(def -conf
  {
   
   })

(def opts
  {:bert.dir/shell "/opt/app/"
   :bert.dir/from-mxnet-example "/opt/app/tmp/data/bert/"
   :bert.dir/python-scripts "/opt/root/python/bert/"
   :bert.dir/mxnet "/root/.mxnet/"
   :bert.python/task "classification"
   :bert.python/seq-length 512
   :bert.python/prefix "bert-cls-4"
   :bert.python/num-classes 4
   :bert.python/output-dir "/opt/app/tmp/data/bert-export/"})

(defn bash-script-fetch-bert-example
  [{:bert.dir/keys [from-mxnet-example]}]
  (format "
  DIR=%s
  mkdir -p $DIR
  cd $DIR

  curl https://s3.us-east-2.amazonaws.com/mxnet-scala/scala-example-ci/BertQA/vocab.json -o vocab.json
  curl https://s3.us-east-2.amazonaws.com/mxnet-scala/scala-example-ci/BertQA/static_bert_qa-0002.params -o static_bert_qa-0002.params
  curl https://s3.us-east-2.amazonaws.com/mxnet-scala/scala-example-ci/BertQA/static_bert_qa-symbol.json -o static_bert_qa-symbol.json
  curl https://s3.us-east-2.amazonaws.com/mxnet-scala/scala-example-ci/BertQA/static_bert_base_net-symbol.json -o static_bert_base_net-symbol.json
  curl https://s3.us-east-2.amazonaws.com/mxnet-scala/scala-example-ci/BertQA/static_bert_base_net-0000.params -o static_bert_base_net-0000.params
  curl https://raw.githubusercontent.com/dmlc/gluon-nlp/master/docs/examples/sentence_embedding/dev.tsv -o dev.tsv
  " from-mxnet-example))

(defn fetch-bert-example
  [{:bert.dir/keys [shell] :as opts}]
  (sh "bash" "-c" (bash-script-fetch-bert-example opts) :dir shell))

#_(fetch-bert-example opts)

(defn bash-script-fetch-bert-python
  [{:bert.dir/keys [mxnet python-scripts ]
    :bert.python/keys [task seq-length prefix num-classes output-dir]}]
  (format "
  DIR_MXNET=%s
  DIR_PYTHON_BERT=%s
  TASK=%s
  SEQ_LENGTH=%s
  PREFIX=%s
  NUM_CLASSES=%s
  OUTPUT_DIR=%s
  
  DIR_MXNET_MODELS=$DIR_MXNET/models

  python3 $DIR_PYTHON_BERT/export/export.py \\
    --task $TASK \\
    --prefix $PREFIX \\
    --seq_length $SEQ_LENGTH \\
    --num_classes $NUM_CLASSES \\
    --output_dir $OUTPUT_DIR

  cd $OUTPUT_DIR
  curl https://s3.us-east-2.amazonaws.com/mxnet-scala/scala-example-ci/BertQA/vocab.json -o vocab.json        
  " mxnet python-scripts task seq-length prefix num-classes output-dir))

(defn fetch-bert-python
  [{:bert.dir/keys [shell] :as opts}]
  (sh "bash" "-c" (bash-script-fetch-bert-python opts) :dir shell))

#_(fetch-bert-python opts)

(defn read-vocab-json!
  [filename]
  (json/parse-stream (io/reader filename)))

(defn read-vocab!
  [filename]
  (with-open [rdr (io/reader filename)]
    (let [lines (line-seq rdr)
          lines-vec (vec lines)]
      {"idx_to_token" lines-vec
       "token_to_idx" (reduce-kv #(assoc %1 %2 %3) {} lines-vec)})))


(defn break-out-punctuation [s str-match]
  (->> (string/split (str s "<punc>") (re-pattern (str "\\" str-match)))
       (map #(string/replace % "<punc>" str-match))))

(defn break-out-punctuations [s]
  (if-let [target-char (first (re-seq #"[.,?!]" s))]
    (break-out-punctuation s target-char)
    [s]))

(defn text>>tokens [s]
  (->> (string/split s #"\s+")
       (mapcat break-out-punctuations)
       (into [])))

(defn pad [tokens pad-item num]
  (if (>= (count tokens) num)
    tokens
    (into tokens (repeat (- num (count tokens)) pad-item))))

(defn tokens>>idxs
  [vocab tokens]
  (let [token-to-idx (get  vocab "token_to_idx")
        idx-unk (get token-to-idx "[UNK]")]
    (mapv #(get token-to-idx % idx-unk) tokens)))

(defn idxs>>tokens
  [vocab idxs]
  (let [idx-to-token (get  vocab "idx_to_token")]
    (mapv #(get idx-to-token %) idxs)))

(defn data>>tokened
  [data item>>text]
  (mapv #(assoc % :tokens (-> % (item>>text) (string/lower-case) (text>>tokens))) data))

(defn data>>padded
  ([data vocab]
   (data>>padded data vocab {}))
  ([data vocab {:keys [seq-length]}]
   (let [max-tokens-length (->> data (mapv #(count (:tokens %))) (apply max))
         seq-length (or seq-length max-tokens-length)]
     (->> data
          (mapv (fn [v]
                  (let [tokens (->> v :tokens (take (- seq-length 2)))
                        valid-length (count tokens)
                        token-types (pad [] 0 seq-length)
                        tokens (->> (concat ["[CLS]"] tokens ["[SEP]"])  (vec))
                        tokens (pad tokens "[PAD]" seq-length)
                        idxs (tokens>>idxs vocab tokens)]
                    (merge v {:batch {:idxs idxs
                                      :token-types token-types
                                      :valid-length [valid-length]}
                              :tokens tokens}))))))))

(defn pair>>padded
  ([pair vocab]
   (pair>>padded pair vocab {}))
  ([pair vocab {:keys [seq-length]}]
   (let [a (first pair)
         b (second pair)
         valid-length (+ (count (:tokens a)) (count (:tokens b)))
         token-a (-> (concat ["[CLS]"] (:tokens a) ["[SEP]"]) (vec))
         token-b (-> (concat (:tokens b) ["[SEP]"]) (vec))
         token-types (into (pad [] 0 (count token-a))
                           (pad [] 1 (count token-b)))
         token-types (pad token-types 0 seq-length)
         tokens (into token-a token-b)
         tokens (pad tokens "[PAD]" seq-length)
         idxs (tokens>>idxs vocab tokens)]
     {:batch {:idxs idxs
              :token-types token-types
              :valid-length [valid-length]}
      :tokens tokens})))

(defn data>>batch-column
  [data column-key]
  (->> data
       (mapv #(-> % :batch column-key))
       (flatten)
       (vec)))