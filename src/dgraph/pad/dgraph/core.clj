(ns tools.dgraph.core
  (:require [clojure.repl :refer :all]
            [cheshire.core :as json]
            [clojure.pprint :as pp]
            [clojure.reflect :refer :all]
   ;
            )
  (:import
           ; (tools.dgraph Example)
   (io.grpc ManagedChannel ManagedChannelBuilder Metadata
            Metadata$Key)
   (io.grpc.stub MetadataUtils)
   (io.dgraph DgraphClient DgraphGrpc
              Transaction  DgraphGrpc$DgraphStub
              DgraphProto$Mutation DgraphProto$Operation
              DgraphProto$Response)
   (com.google.gson Gson)
   (com.google.protobuf ByteString)
   ;
   )
  ;
  )


(comment


  ; (Example/run)

  ; (Example/prn "asd")


  ; (Example/main)

  ; (Example/prn)

;  
  )

(defn create-client
  "create DgraphClient"
  [{:keys [with-auth-header?
           hostname
           port]}]
  (let [ch   (->
              (ManagedChannelBuilder/forAddress hostname port)
              (.usePlaintext true)
              (.build))
        stub (DgraphGrpc/newStub ch)]
    (cond
      with-auth-header? (let [md   (->
                                    (Metadata.)
                                    (.put
                                     (Metadata$Key/of "auth-token" Metadata/ASCII_STRING_MARSHALLER)
                                     "the-auth-token-value"))
                              stub (MetadataUtils/attachHeaders stub md)]
                          (DgraphClient. (into-array [stub])))
      :else (DgraphClient. (into-array [stub]))
      ; :else stub
      )))


(defn q-res
  "returns a Response protocol buffer object "
  [{:keys [client
           qstring
           vars]}]
  (let [res (->
             (.newTransaction client)
             (.queryWithVars qstring vars))]
    res))

(defn q-json
  "returns dgraph json response"
  [{:keys [client
           qstring
           vars]}]
  (let [res (->
             (.newTransaction client)
             (.queryWithVars qstring vars))
        res-json (.getJson res)]
    res-json))

(defn q-edn
  "returns dgraph json response"
  [{:keys [client
           qstring
           vars]}]
  (let [res (->
             (.newTransaction client)
             (.queryWithVars qstring vars))
        res-str (->
                 (.getJson res)
                 (.toStringUtf8))
        res-edn (json/parse-string res-str)]
    res-edn))

(defn res->str
  "Returns Response protobuf object to string"
  [res]
  (->
   (.getJson res)
   (.toStringUtf8)))

(defn q
  "Queries Dgraph"
  [opts]
  (->
   (q-res opts)
   (res->str)
   (json/parse-string)))

(defn qry
  [qstring client & {:keys [vars]
                     :or   {vars {}}}]
  (->
   (q {:qstring qstring
       :client  client
       :vars    vars})
   (pp/pprint)))

(defn count-total-nodes
  [c]
  (->
   (q {:qstring "
     {
  total (func: has (_predicate_) ) {
    count(uid)
  }
} 
      "
       :client  c
       :vars    {}})
   (pp/pprint)))


(comment

  (def c (create-client {:with-auth-header? false
                         :hostname          "server"
                         :port              9080}))

  (count-total-nodes c)
  ;
  )


(defn mutate
  "Transact dgraph mutation"
  [{:keys [data client]}]
  (let [txn (.newTransaction client)]
    (try
      (let [mu  (->
                 (DgraphProto$Mutation/newBuilder)
                 (.setSetJson (ByteString/copyFromUtf8 (json/generate-string data)))
                 (.build))]
        (.mutate txn mu)
        (.commit txn))
      (catch Exception e (str "caught exception: " (.getMessage e)))
      (finally (.discard txn)))
    ;
    ))

(defn mutate-del
  "Transact dgraph mutation"
  [{:keys [s client]}]
  (let [txn (.newTransaction client)]
    (try
      (let [mu  (->
                 (DgraphProto$Mutation/newBuilder)
                 (.setDelNquads (ByteString/copyFromUtf8 s))
                 (.build))]
        (.mutate txn mu)
        (.commit txn))
      (catch Exception e (str "caught exception: " (.getMessage e)))
      (finally (.discard txn)))
    ;
    ))

; https://github.com/dgraph-io/dgraph4j#alter-the-database
; https://github.com/dgraph-io/dgraph/pull/1547
(defn drop-all
  "Drop all"
  [client]
  (let [op (->
            (DgraphProto$Operation/newBuilder)
            (.setDropAll true)
            (.build))]
    (.alter client op)))


(defn set-schema
  "Set the dgraph schema"
  [{:keys [schema-string
           client]}]
  (let [op (->
            (DgraphProto$Operation/newBuilder)
            (.setSchema schema-string)
            (.build))]
    (.alter client op)))