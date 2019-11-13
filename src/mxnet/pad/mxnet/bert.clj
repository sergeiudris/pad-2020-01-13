(ns pad.mxnet.bert
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cheshire.core :as json]))

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