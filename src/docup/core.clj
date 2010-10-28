(ns ^{:doc "A simple, non-cluttered markup language"
      :author "Andrew Gwozdziewycz"}
  docup.core
  (:use [clojure.string :only (join triml)])
  (:import [java.io StringReader]))

(def ^{:private true} exploring-count 1000)

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
   (repeatedly c #(char* (.read *in*)))
   (finally 
    (.reset *in*))))

(defn- peek-until [ch]
  (try
   ;; we want an upper bound on what we can explore
   (.mark *in* exploring-count)
   (loop [left exploring-count
          buff []]
     (if (> left 0)
       (let [c (read-char)]
         (if (or (= c :eof)
                 (= c ch))
           (join buff)
           (recur (dec left) (conj buff c))))
       (join buff)))
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
                          (read-char)
                          c)))
        :eof (join buff)
        (recur (conj buff c))))))

(defn maybe-read-image
  "Maybe reads an image, given starting char, otherwise whatever is read
 is returned as buffer"
  []
  (let [label? (peek-until \:)
        desc? (peek-until \])]
    (if (and (< (count label?) (count desc?))
             (> (count label?) 0))
      (let [label (read-until \:)
            desc (let [_ (read-char)
                       t (triml (read-until \]))
                       _ (read-char)]
                   t)]
        [:img label desc])
      nil)))

(def ^{:private true} char-tags {\* :b \` :code \/ :i \_ :u})
(def ^{:private true} reserved-char (apply hash-set (keys char-tags)))

(defn read-paragraph []
  (loop [buff []
         accum [:p]]
    (let [c (read-char)
          tag (char-tags c)]
      (cond
        (= c :eof) (if (> (count buff) 0)
                     (conj accum (join buff))
                     accum)
        (= c \[) (let [img? (maybe-read-image)]
                   (if img?
                     (recur [] (continue-reading buff accum #(identity img?)))
                     (recur (conj buff c) accum)))
        (not (nil? tag)) (recur [] (continue-reading buff accum
                                    #(vector tag (read-matching c)))) 
       :else (recur (conj buff c) accum)))))

(defn docup
  "Returns a tree of the represented document"
  ([] (read-paragraph))
  ([s]
     (binding [*in* (StringReader. s)]
       (docup))))
