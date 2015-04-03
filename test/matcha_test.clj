(ns matcha_test
  (:require [matcha]
            [clojure.test :refer [deftest]]))

(defn check-successful
  "asserts that a given matcher succeeds, given a value"
  [matcher a]
  (let [result (matcha/run-match matcher a)]
    (clojure.test/is (:pass? result) (str "matcha:\n" (matcha/format-message result)))))

(defn check-failure
  "asserts that a given matcher fails given a value"
  [matcher a]
  (let [result (matcha/run-match matcher a)]
    (clojure.test/is (not (:pass? result)) (str "matcha:\n" (matcha/format-message result)))))

(deftest =-test
  (check-successful (matcha/= 1) 1)
  (check-failure (matcha/= 1) 0))

(deftest empty?-test
  (check-successful matcha/empty? [])
  (check-failure matcha/empty? [1]))

(deftest all-of-test
  (check-successful (matcha/all-of (matcha/= 1) (matcha/= 1)) 1)
  (check-failure (matcha/all-of (matcha/= 2) (matcha/= 1)) 1))

(deftest any-of-test
  (check-successful (matcha/any-of (matcha/= 1) (matcha/= 2)) 2)
  (check-failure (matcha/any-of (matcha/= 1) (matcha/= 2)) 3))

(deftest has-count
  (check-successful (matcha/has-count 0) [])
  (check-failure (matcha/has-count 1) []))

(deftest some-test
  (check-successful (matcha/some (matcha/= 1)) [1])
  (check-failure (matcha/some (matcha/= 2)) [1]))

(deftest every?-test
  (check-successful (matcha/every? (matcha/= 1)) [1 1])
  (check-failure (matcha/every? (matcha/= 1)) [1 2]))

(deftest not-test
  (check-successful (matcha/not (matcha/= 1)) 2)
  (check-failure (matcha/not (matcha/= 2)) 2))

(deftest <=-test
  (check-successful (matcha/<= 1) 1)
  (check-successful (matcha/<= 1) 2)
  (check-failure (matcha/<= 3) 2))

(deftest >=-test
  (check-successful (matcha/>= 1) 1)
  (check-successful (matcha/>= 1) 0)
  (check-failure (matcha/>= 1) 2))

(deftest <-test
  (check-successful (matcha/< 1) 2)
  (check-failure (matcha/< 3) 2))

(deftest >-test
  (check-successful (matcha/> 1) 0)
  (check-failure (matcha/> 1) 2))

(deftest instance?-test
  (check-successful (matcha/instance? clojure.lang.Keyword) :foo)
  (check-failure (matcha/instance? clojure.lang.Keyword) 1))

(deftest string?-test
  (check-successful matcha/string? "1")
  (check-failure matcha/string? 1))

(deftest map?-test
  (check-successful matcha/map? {})
  (check-failure matcha/map? 1))

(deftest seq?-test
  (check-successful matcha/seq? '())
  (check-failure matcha/seq? 1))

(deftest char?-test
  (check-successful matcha/char? \a)
  (check-failure matcha/char? 1))

(deftest vector?-test
  (check-successful matcha/vector? [])
  (check-failure matcha/vector? 1))

(deftest keyword?-test
  (check-successful matcha/keyword? :foo)
  (check-failure matcha/keyword? 1))

(deftest symbol?-test
  (check-successful matcha/symbol? 'foo)
  (check-failure matcha/symbol? 1))

(deftest nil?-test
  (check-successful matcha/nil? nil)
  (check-failure matcha/nil? 1))
