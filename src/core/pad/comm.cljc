(ns pad.comm)

(defn resolve-var
  [sym]
  (var-get (resolve sym)))