(ns chartem_test
  (:require [chartem]
            [clojure.test :refer [deftest]]))

(defn check-successful
  "asserts that a given matcher succeeds, given a value"
  [matcher a]
  (let [result (chartem/run-match matcher a)]
    (clojure.test/is (:pass? result) (str "CHARTEM:\n" (chartem/format-message result)))))

(defn check-failure
  "asserts that a given matcher fails given a value"
  [matcher a]
  (let [result (chartem/run-match matcher a)]
    (clojure.test/is (not (:pass? result)) (str "CHARTEM:\n" (chartem/format-message result)))))

(deftest =-test
  (check-successful (chartem/= 1) 1)
  (check-failure (chartem/= 1) 0))

(deftest empty?-test
  (check-successful chartem/empty? [])
  (check-failure chartem/empty? [1]))

(deftest all-of-test
  (check-successful (chartem/all-of (chartem/= 1) (chartem/= 1)) 1)
  (check-failure (chartem/all-of (chartem/= 2) (chartem/= 1)) 1))

(deftest any-of-test
  (check-successful (chartem/any-of (chartem/= 1) (chartem/= 2)) 2)
  (check-failure (chartem/any-of (chartem/= 1) (chartem/= 2)) 3))

(deftest has-count
  (check-successful (chartem/has-count 0) [])
  (check-failure (chartem/has-count 1) []))

(deftest some-test
  (check-successful (chartem/some (chartem/= 1)) [1])
  (check-failure (chartem/some (chartem/= 2)) [1]))

(deftest every?-test
  (check-successful (chartem/every? (chartem/= 1)) [1 1])
  (check-failure (chartem/every? (chartem/= 1)) [1 2]))

(deftest not-test
  (check-successful (chartem/not (chartem/= 1)) 2)
  (check-failure (chartem/not (chartem/= 2)) 2))

(deftest <=-test
  (check-successful (chartem/<= 1) 1)
  (check-successful (chartem/<= 1) 2)
  (check-failure (chartem/<= 3) 2))

(deftest >=-test
  (check-successful (chartem/>= 1) 1)
  (check-successful (chartem/>= 1) 0)
  (check-failure (chartem/>= 1) 2))

(deftest <-test
  (check-successful (chartem/< 1) 2)
  (check-failure (chartem/< 3) 2))

(deftest >-test
  (check-successful (chartem/> 1) 0)
  (check-failure (chartem/> 1) 2))
