(ns pad.coll.core)

(defn ping [] 'pong)

(defn drop-nth
  "Remove nth element from a coll"
  [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn partition-into-vecs
  "Returns vec of vecs "
  [part-size v]
  (->>
   (partition part-size v)
   (mapv vec)))

(defn nth-seq
  "Returns the nth element in a seq"
  [coll index]
  (first (drop index coll)))

(defn contained?
  [v coll]
  (some #(= v %) coll))