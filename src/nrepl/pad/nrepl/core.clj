(ns pad.nrepl.core
  (:require [nrepl.server :refer [start-server stop-server]]
            [clojure.repl :refer :all]
   ;
            ))

(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defn start-nrepl-server [& {:keys [host port] :or {port 7888 host "0.0.0.0"} }]
  (prn "--started nREPL server on 7888 ")
  (start-server
   :bind host
   :port port
   :handler (nrepl-handler)
   :middleware '[]))

