(ns pad.nrepl.core
  (:require [nrepl.server :refer [start-server stop-server]]
            [clojure.repl :refer :all]
            [cider.nrepl :refer [cider-nrepl-handler]]
   ;
            ))

#_(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defn start-nrepl-server [& {:keys [host port] :or {port 7888 host "0.0.0.0"}}]
  (prn (str "--started nREPL server on " port))
  (start-server
   :bind host
   :port port
   :handler cider-nrepl-handler #_(nrepl-handler)
   :middleware '[]))

