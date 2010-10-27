(ns docup.core
  (:use [clojure.string :only (join)])
  (:import [java.io StringReader]))

(defn- char* [c]
  (if (== c -1) :eof (char c))) 

(defn- read-char []
  (char* (.read *in*)))

(defn- peek-char []
  (try
   (.mark *in* 1)
   (char* (.read *in*))
   (finally
    (.reset *in*))))

(defn- peek-chars [c]
  (try
   (.mark *in* c)
   (loop [buff []]
     (repeatedly c #(char* (.read *in*))))
   (finally 
    (.reset *in*))))

(defn- read-until [ch]
  (loop [buff []]
    (let [c (peek-char)]
      (if (or (= c :eof) 
              (= c ch))
        (join buff)
        (recur (conj buff (read-char)))))))

(defn- continue-reading
  [buff tree f]
  (if (> (count buff) 0)
    (conj (conj tree (join buff)) (f))
    (conj tree (f))))

(defn read-matching [til]
  (loop [buff []]
    (let [c (read-char)]
      (condp = c
        til (join buff)
        \\ (if (= (peek-char) til)
             (recur (conj buff til))
             (recur (conj buff c)))
        :eof (join buff)
        (recur (conj buff c))))))

(defn read-paragraph []
  (loop [buff []
         accum [:p]]
    (let [c (read-char)]
      (case c
        \* (recur [] (continue-reading buff accum 
                                       #(vector :b (read-matching c))))
        \` (recur [] (continue-reading buff accum
                                       #(vector :code (read-matching c))))
        \/ (recur [] (continue-reading buff accum
                                       #(vector :i (read-matching c))))
        \_ (recur [] (continue-reading buff accum
                                       #(vector :u (read-matching c))))
        :eof (if (> (count buff) 0)
               (conj accum (join buff))
               accum)
        (recur (conj buff c) accum)))))

(defn docup []
  "Returns a tree of the represented document"
  (read-paragraph))

(defn docup-str
  "Returns a tree of the represented document"
  [s]
  (binding [*in* (StringReader. s)]
    (docup)))