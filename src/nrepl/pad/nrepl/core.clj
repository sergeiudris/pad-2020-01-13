(ns pad.nrepl.core
  (:require [nrepl.server :refer [start-server stop-server]]
            [clojure.repl :refer :all]
            ; [whidbey.repl]
   ;
            ))

(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

; (defn wrap-print
;   [handler]
;   (fn [{:keys [::options] :as msg}]
;     (prn msg)
;     (handler {})))

(defn -main []
  (prn "--running REPL on 7878 ")
  (defonce server (start-server
                   :bind "0.0.0.0"
                   :port 7878
                   :handler (nrepl-handler)
                   :middleware '[;  cider.nrepl.middleware.apropos/wrap-apropos
                                ;  cider.nrepl.middleware.version/wrap-version
                                ;  cider.nrepl.middleware.out/wrap-out
                                ;  cider.nrepl.middleware.trace/wrap-trace
                                ;  cider.nrepl.middleware.complete/wrap-complete
                                ;  cider.nrepl.middleware.stacktrace/wrap-stacktrace
                                ;  cider.nrepl.middleware.format/wrap-format
                                ;  cider.nrepl.middleware.refresh/wrap-refresh
                                ;  cider.nrepl.middleware.ns/wrap-ns
                                ;  cider.nrepl.middleware.undef/wrap-undef

                                ;  nrepl.middleware.print/wrap-print


                                ;  cider.nrepl/wrap-apropos
                                ;  cider.nrepl/wrap-classpath
                                ;  cider.nrepl/wrap-complete
                                ;  cider.nrepl/wrap-debug
                                ;  cider.nrepl/wrap-format
                                ;  cider.nrepl/wrap-info
                                ;  cider.nrepl/wrap-inspect
                                ;  cider.nrepl/wrap-macroexpand
                                ;  cider.nrepl/wrap-ns
                                ;  cider.nrepl/wrap-spec
                                ;  cider.nrepl/wrap-profile
                                ;  cider.nrepl/wrap-refresh
                                ;  cider.nrepl/wrap-resource
                                ;  cider.nrepl/wrap-stacktrace
                                ;  cider.nrepl/wrap-test
                                ;  cider.nrepl/wrap-trace
                                ;  cider.nrepl/wrap-out
                                ;  cider.nrepl/wrap-undef
                                ;  cider.nrepl/wrap-print
                                ;  cider.nrepl/wrap-version
                                 ]))

  ; (whidbey.repl/init! {:print-color     true
  ;                     ;  :map-delimiter   ""
  ;                     ;  :extend-notation true
  ;                     ;  :print-meta      true
  ;                      })
  ; (whidbey.repl/update-print-fn!)
  ;
  )

(comment

  (+ 1 1)
  (def nrepl.middleware.print/*print-fn* 1)


    ; (whidbey.repl/init! {:print-color true
    ;                      :width           180
    ;                   ;  :map-delimiter   ""
    ;                   ;  :extend-notation true
    ;                   ;  :print-meta      true
    ;                      })
  ; (whidbey.repl/update-print-fn!)

  {:width           180
   :map-delimiter   ""
   :extend-notation true
   :print-meta      true
   :color-scheme    {:delimiter [:blue]
                     :tag       [:bold :red]}}
  ;
  )