(ns pad.io.core
  (:require [clojure.repl :refer :all]
            [clojure.reflect :refer :all]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.string :as str]
            ;
            ))

(defn read-nth-line
  "Read line-number from the given text file. The first line has the number 1."
  [filename line-number]
  (with-open [rdr (clojure.java.io/reader filename)]
    (nth (line-seq rdr) (dec line-number))))

(defn read-n-lines
  "Read n lines from the given text file"
  ([filename]
   (read-n-lines filename 0 5))
  ([filename count]
   (read-n-lines filename 0 count))
  ([filename offset count]
   (with-open [rdr (clojure.java.io/reader filename)]
     (->> (line-seq rdr)
          (drop offset)
          (take count)
          (vec)))))

(defn pprn-n-lines
  "read-n-lines , pp/print"
  [& args]
  (->> args
       (apply read-n-lines)
       (pp/pprint)))

(defn count-lines
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (count (line-seq rdr))))

(defn write-lines
  "Write lines vector to file"
  [filename lines-vec]
  (with-open [w (clojure.java.io/writer filename :append true)]
    (doseq [line lines-vec]
      (.write w (str line "\n")))))

(defn delete-files
  [& filenames]
  (doseq [filename filenames]
    (.delete (java.io.File. filename))))

(defn mk-dirs
  "Make directories in the path"
  [path]
  (.mkdirs (java.io.File. path)))

(defn create-file
  [filename]
  (.createNewFile (java.io.File. filename)))

(defn read-binary-file
  "Returns byte-array , accepts :offset :limit"
  [filename & {:keys [offset limit]
               :or   {offset 0}}]
  (let [f   (io/file filename)
        lim (or limit (- (.length f) offset))
        buf (byte-array lim)]
    ; (prn lim offset)
    (with-open [in (io/input-stream  f)]
      (.skip in offset)
      (.read in buf 0 lim)
      buf
      ;
      )))

(defn bytes->int [bytes]
  "Converts a byte array into an integer."
  (->>
   bytes
   (map (partial format "%02x"))
   (apply (partial str "0x"))
   read-string))

(defn read-int-from-file
  "Returns the number represented by bytes in range [offset , + offset  limit]"
  [filename offset limit]
  (->   (read-binary-file  filename :offset offset :limit limit)
        bytes->int))

