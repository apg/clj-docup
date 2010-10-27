(ns docup.test.core
  (:use [docup.core] :reload)
  (:use [clojure.test]))

(defmacro docup-is [s coll]
  `(is (docup-str ~s) ~coll))

(deftest test-bold
  (docup-is "*hello world*" [:p [:b "hello world"]])
  (docup-is "*hello\\* world*" [:p [:b "hello* world"]])
  (docup-is "*hello _underlined_ world*" [:p [:b "hello _underlined_ world"]]))

(deftest test-italics
  (docup-is "/hello world/" [:p [:i "hello world"]])
  (docup-is "/hello\\` world/" [:p [:i "hello_ world"]]))

(deftest test-underline
  (docup-is "_hello world/" [:p [:i "hello world"]])
  (docup-is "_hello\\_ world_" [:p [:i "hello_ world"]]))

(deftest test-code
  (docup-is "`hello world`" [:p [:code "hello world"]])
  (docup-is "`hello\\` world`" [:p [:code "hello` world"]]))
