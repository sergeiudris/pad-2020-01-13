(ns pad.dataset.bert
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
   
   })

(def opts
  {:dir/shell "/opt/app/"
   :dir/bert-export "/opt/app/tmp/data/bert-export/"
   :dir/bert-example "/opt/app/tmp/data/bert/"
   :dir/python-bert "/opt/root/python/bert/"
   :dir/.mxnet "/root/.mxnet/"
   })

(defn script-fetch-bert-example
  [{:dir/keys [bert-example]}]
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
  " bert-example))

(defn fetch-bert-example
  [{:dir/keys [shell] :as opts}]
  (let [script (script-fetch-bert-example opts)]
    (sh "bash" "-c" script :dir shell)))

(defn script-bert-python
  [{:dir/keys [.mxnet python-bert bert-export]
    :keys [task seq-length prefix num-classes]}]
  (format "
  DIR_MXNET=%s
  DIR_PYTHON_BERT=%s
  TASK=%s
  SEQ_LENGTH=%s
  PREFIX=%s
  NUM_CLASSES=%s
  OUTPUT_DIR=%s
  
  DIR_MXNET_MODELS=$DIR_MXNET/models

  python $DIR_PYTHON_BERT/export/export.py \
    --task $TASK \
    --prefix $PREFIX \
    --seq_length $SEQ_LENGTH \
    --num_classes $NUM_CLASSES \
    --output_dir $OUTPUT_DIR
  " .mxnet python-bert task seq-length prefix num-classes bert-export))

(defn fetch-bert-example
  [{:dir/keys [shell] :as opts}]
  (let [script (script-bert-python opts)]
    (sh "bash" "-c" script :dir shell)))
