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

(deftest every?-test
  (check-successful (chartem/every? (chartem/= 1) (chartem/= 1)) 1)
  (check-failure (chartem/every? (chartem/= 2) (chartem/= 1)) 1))

(deftest some-test
  (check-successful (chartem/some (chartem/= 1) (chartem/= 2)) 2)
  (check-failure (chartem/some (chartem/= 1) (chartem/= 2)) 3))
