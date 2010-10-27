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
        \\ (recur (conj buff 
                        (if (= (peek-char) til)
                          til
                          c)))
        :eof (join buff)
        (recur (conj buff c))))))

(def ^{:private true} char-tags {\* :b \` :code \/ :i \_ :u})

(defn read-paragraph []
  (loop [buff []
         accum [:p]]
    (let [c (read-char)
          tag (char-tags c)]
      (cond
       (not (nil? tag)) (continue-reading buff accum
                                          #(vector tag (read-matching c)))
       :eof (if (> (count buff) 0)
              (conj accum (join buff))
              accum)
       :else (recur (conj buff c) accum)))))

(defn docup
  "Returns a tree of the represented document"
  ([] (read-paragraph))
  ([s]
     (binding [*in* (StringReader. s)]
       (docup))))
