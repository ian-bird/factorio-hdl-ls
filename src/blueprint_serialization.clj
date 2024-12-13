(ns blueprint-serialization
  (:require
   [clojure.data.codec.base64 :as b64]
   [clojure.string :as str]
   [cheshire.core :as chess]))

(import '[java.io ByteArrayOutputStream ByteArrayInputStream]
        '[java.util.zip InflaterInputStream]
        '[java.util.zip DeflaterOutputStream])


(defn- deflate-bytes [input-bytes]
  (with-open [byte-in (ByteArrayInputStream. input-bytes)
              byte-out (ByteArrayOutputStream.)
              deflater-out (DeflaterOutputStream. byte-out)]
    (let [buffer (byte-array 1024)]
      (loop []
        (let [n (.read byte-in buffer)]
          (when (pos? n)
            (.write deflater-out buffer 0 n)
            (recur))))
      (.finish deflater-out)
      (.toByteArray byte-out))))

(defn- inflate-bytes [compressed-bytes]
  (with-open [byte-in (ByteArrayInputStream. compressed-bytes)
              inflater-in (InflaterInputStream. byte-in)
              byte-out (ByteArrayOutputStream.)]
    (let [buffer (byte-array 1024)]
      (loop []
        (let [n (.read inflater-in buffer)]
          (when (pos? n)
            (.write byte-out buffer 0 n)
            (recur))))
      (.toString byte-out "UTF-8"))))

(defn from-sexpr [sexpr]
  (->> sexpr
       chess/generate-string
       .getBytes
       deflate-bytes
       b64/encode
       String.
       (list "0")
       str/join))

(defn to-sexpr
  [blueprint-string]
  (-> blueprint-string
      (subs 1)
      .getBytes
      b64/decode
      inflate-bytes
      String.
      chess/parse-string))