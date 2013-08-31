(ns tomdog.core
  (:require [clojure.string :as str :refer [trim join]]
            [clojure.pprint :as pprint])
  (:import [java.net ServerSocket Socket]
           [java.io InputStream OutputStream File]))

(set! *warn-on-reflection* true)

(defn read-request-body 
  "read request body from the input stream"
  [^InputStream istream]
  (let [^bytes buffer (byte-array 1024)
        ^StringBuilder ret (StringBuilder.)]
    (loop [ret ret]
      ;; request body ends with two '\r\n'
      (if (not (.endsWith (.toString ret) "\r\n\r\n"))
        (let [cnt (.read istream buffer)
              line (String. buffer 0 cnt)]
          (when (> cnt 2)
            (.append ret (String. buffer 0 cnt))
            (recur ret)))
        (.toString ret)))))

(defn decode-request-first-line 
  "decode the first line of a http request(GET / HTTP/1.1)"
  [^String line]
  (let [idx (.indexOf line " ")
        last-idx (.lastIndexOf line " ")
        method (subs line 0 idx)
        url (subs line (inc idx) last-idx)
        url (if (.endsWith url "\n")
              (subs url 0 (dec (count url)))
              url)]
    {:method method
     :url url}))

(defn decode-request-header 
  "decode request header(Host: localhost:9999)"
  [^String line]
  (let [idx (.indexOf line ":")
        head-name (trim (subs line 0 idx))
        head-value (trim (subs line (inc idx) (count line)))]
    {head-name head-value}))

(defn decode-request 
  "decode request"
  [^String request-str]
  (println "begin decode request..." request-str)
  (let [request-arr (vec (.split request-str "\r\n"))
        request (decode-request-first-line (first request-arr))
        request (into request (for [line (rest request-arr)]
                                (do (println "A: " line)
                                    (if (= line "\r\n")
                                      nil
                                      (decode-request-header line)))
                                ))]
    request))

(defn determine-content-type 
  "determine content type from file extension"
  [^String path]
  (let [idx (.lastIndexOf path ".")
        extension (if (pos? idx)
                    (subs path (inc idx) (count path))
                    nil)]
    (case extension
      "html" "text/html"
      "htm" "text/html"
      "vm" "text/html"
      "xml" "text/xml"
      "json" "application/json"
      "jpg" "image/jpeg"
      "gif" "image/gif"
      "png" "image/png"
      "css" "text/css"
      "text/plain")))

(defn read-file-content 
  "read the file from /tmp directory"
  [dir path]
  (let [real-path (str dir path)
        ^File real-file (File. real-path)]
    (if (not (.exists real-file))
      {:code 404
       :code-text "Not Found"
       :content-type (determine-content-type path)
       :content "404 Not Found"}
      (if (.isFile real-file)
        {:code 200
         :code-text "OK"
         :content-type (determine-content-type path)
         :content (slurp real-path)}
        (let [sub-files (.listFiles real-file)
              parent-file (.getParentFile real-file)
              parent-path (.getPath parent-file)
              parent-path (if (< (- (count parent-path) (count dir)) 2)
                            "/"
                            (subs parent-path (count dir) (count parent-path)))
              ret [(str "<ul><li><a href='" parent-path "'>..</a></li>")]
              ret (into ret (for [^File f sub-files
                                  :let [fpath (.getPath f)
                                        fpath-desc (.getName f)
                                        fpath (subs fpath (count dir) (count fpath))]]
                              (str "<li><a href='" fpath "'>" fpath-desc "</a></li>")))
              ret (conj ret "</ul>")
              ret (join "" ret)]
          {:code 200
           :code-text "OK"
           :content-type "text/html"
           :content ret})))))

(defn write-bytes 
  "write the input string as bytes into output stream"
  [^OutputStream ostream ^String str1]
  (.write ostream (.getBytes str1)))


(defn write-response 
  "write response into the output stream"
  [^OutputStream ostream response]
  (let [response-bytes (.getBytes (:content response))
        cnt (count response-bytes)]
    (write-bytes ostream (str "HTTP/1.1 " (:code response) " " (:code-text response) "\r\n"))
    (write-bytes ostream "Date: Wed, 28 Aug 2013 07:36:30 GMT\r\n")
    (write-bytes ostream "Server: Tomdog\r\n")
    (write-bytes ostream (str "Content-Type: " (:content-type response) "\r\n") )
    (write-bytes ostream (str "Content-Length: " cnt "\r\n"))
    (write-bytes ostream "\r\n")
    (.flush ostream)    
    (.write ostream response-bytes)
    (.flush ostream)))

(defn handle-request 
  "handle a http request"
  [^Socket socket root-path]
  (let [^InputStream istream (.getInputStream socket)
        ^OutputStream ostream (.getOutputStream socket)
        request-body (read-request-body istream)
        request (decode-request request-body)
        response (read-file-content root-path (:url request))]
    (println "request is: ")
    (pprint/pprint request)
    (println)
    (write-response ostream response)
    (.close ostream)
    (.close istream)))

(defn server 
  "tomdog"
  [port root-path]
  (let [server (ServerSocket. port)]
    (println "Server started! Serving path: " root-path " at port " port)
    (loop [^Socket socket (.accept server)]
      (future (handle-request socket root-path))
      (recur (.accept server)))))

(defn -main [& args]
  (let [params (partition 2 args)
        params (into {:port 9999
                      :root "."}
                     (for [pair params]
                       (case (first pair)
                         "-p" [:port (Integer/parseInt (second pair))]
                         "-d" [:root (second pair)])))]
    (println "params: " params)
    (server (:port params) (:root params))))

