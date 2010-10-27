(ns docup.test.core
  (:use [docup.core] :reload)
  (:use [clojure.test]))

(deftest test-bold
  (is (docup "*hello world*") [:p [:b "hello world"]])
  (is (docup "*hello\\* world*") [:p [:b "hello* world"]])
  (is (docup "*hello _underlined_ world*") [:p [:b "hello _underlined_ world"]]))

(deftest test-italics
  (is (docup "/hello world/") [:p [:i "hello world"]])
  (is (docup "/hello\\/ world/") [:p [:i "hello/ world"]])
  (is (docup "/hello _underlined_ world/") [:p [:i "hello _underlined_ world"]]))

(deftest test-code
  (is (docup "`hello world`") [:p [:code "hello world"]])
  (is (docup "`hello\\` world`") [:p [:code "hello` world"]])
  (is (docup "`hello _underlined_ world`") [:p [:code "hello _underlined_ world"]]))

(deftest test-underline
  (is (docup "_hello world_") [:p [:b "hello world"]])
  (is (docup "_hello\\_ world*") [:p [:b "hello* world"]])
  (is (docup "_hello `underlined` world_") [:p [:b "hello `underlined` world"]]))

