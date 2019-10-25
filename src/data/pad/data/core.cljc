(ns pad.data.core)

(defn rand-int-in-range
  "returns random int in range a b"
  [a b]
  (int (- b (* (rand) (- b a)))))

