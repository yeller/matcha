(ns matcha
  (:refer-clojure
    :exclude
    [empty? every?
     = some not <= >= < >
     instance? string? map? seq? char? vector? nil? keyword? symbol? ratio? decimal? float? isa? rational? coll? set? list?])
  (:require [clojure.string :as string]
            [clojure.core :as core]))
;; Matcher
;; Match
;;
;; is :: a -> Matcher a
;; allOf [Matcher a] -> Matcher a
;; runMatcher -> Matcher a -> a -> Match

;; a matcher is a map with three fields, each of whose values are functions:
;; {:match ;; a -> Bool
;;  :description :: String
;;  :describe-mismatch :: a -> String }
;;
;;  a match is a map with pass? and message, message will be a string
;;  if pass? if false
;;
;;  TODO: matchers list
;;  - all the matchers using `on` should have good error messages with bad types
;;  - fn?
;;  - re-matches
;; -- ** Matcher combinators
;; , and-also
(defn describe-list [call xs]
  (str "(" call " " (clojure.string/join " " xs) ")"))

(defn standard-describe-mismatch [x]
  (pr-str x))

(defn describe-mismatch-feature [x feature-name feature-mismatch]
  (str (standard-describe-mismatch x) " with " feature-name " " feature-mismatch))

(defn on
  "combinator. Passes if the (f the-value) matches the matcher"
  ([f m feature-name]
   (on f m feature-name "something"))
  ([f m feature-name class-name]
   {:match
    (fn [a]
      ((:match m) (f a)))
    :description
    (str class-name " with " feature-name " " (:description m))
    :describe-mismatch
    (fn [a]
      (describe-mismatch-feature a feature-name ((:describe-mismatch m) (f a)))) }))

(defn type-matcher
  "checks the value matches a predicate before matching
  using the matcher given."
  [pred type-name m]
  {:match (fn match [x]
            (and (pred x)
                 ((:match m) x)))
   :description (:description m)
   :describe-mismatch #(str "not a " type-name ", " (describe-class-mismatch %))})

(defn =
  "matches based on equality of the value given

  (matcha/run-match (matcha/= 1) 1) ; => passes
  (matcha/run-match (matcha/= 1) 2) ; => fails"
  [a]
  {:match (fn [b] (core/= a b))
   :description (pr-str a)
   :describe-mismatch standard-describe-mismatch})

(defn <=
  "matches based if the value given is greater-than or equal to

  (matcha/run-match (matcha/<= 1) 1) ; => passes
  (matcha/run-match (matcha/<= 1) 0) ; => fails"
  [a]
  {:match (fn [b] (core/<= a b))
   :description (str "less than or equal to " (pr-str a))
   :describe-mismatch standard-describe-mismatch})

(defn <
  "matches based if the value given is greater-than or equal to

  (matcha/run-match (matcha/< 1) 2) ; => passes
  (matcha/run-match (matcha/< 1) 0) ; => fails"
  [a]
  {:match (fn [b] (core/< a b))
   :description (str "less than " (pr-str a))
   :describe-mismatch standard-describe-mismatch})

(defn >
  "matches based if the value given is greater-than

  (matcha/run-match (matcha/> 1) 2) ; => passes
  (matcha/run-match (matcha/> 1) 0) ; => fails"
  [a]
  {:match (fn [b] (core/> a b))
   :description (str "more than " (pr-str a))
   :describe-mismatch standard-describe-mismatch})

(defn >=
  "matches based if the value given is less-than or equal to

  (matcha/run-match (matcha/<= 1) 1) ; => passes
  (matcha/run-match (matcha/<= 1) 0) ; => fails"
  [a]
  {:match (fn [b] (core/>= a b))
   :description (str "less than or equal to " (pr-str a))})

(defn has-numerator
  "passes if the ratio has the numerator given
   (matcha/run-match (matcha/has-numerator 1) 1/10) ; => passes
   (matcha/run-match matcha/nil? 1) ; => fails"
  [n]
  (type-matcher
    core/ratio?
    "a ratio"
    (on numerator (= n) "numerator" "a number")))

(defn has-denominator
  "passes if the ratio has the denominator given
   (matcha/run-match (matcha/has-numerator 1) 1/10) ; => passes
   (matcha/run-match matcha/nil? 1) ; => fails"
  [n]
  (type-matcher
    core/ratio?
    "a ratio"
    (on denominator (= n) "denominator" "a number")))

(def empty?
  "matches if the collection passed is empty

  (matcha/run-match matcha/empty? [1]) ; => passes
  (matcha/run-match matcha/empty? [])  ; => fails"
  {:match core/empty?
   :description "an empty collection"})

(defn format-message
  "turns the results of a failing match into a human readable error message,
   suitable for printing with clojure.core/print or clojure.core/println"
  [result]
  (assert (clojure.core/not (:pass? result true)) (str "format message should only be used with a failing result, but it was given: " (pr-str result)))
  (str "\nExpected: " (:expected result)
       "\n but was: "
       (:was result)))

(defn all-of
  "matches if all of the matchers given pass:

  (matcha/run-match (matcha/all-of (matcha/= 1) (matcha/= 1)) 1) ; => passes
  (matcha/run-match (matcha/all-of (matcha/= 1) (matcha/= 1)) 2) ; => fails"
  [& ms]
  {:match
   (fn [a] (reduce (fn [x y] (and x y))
                          (map #((:match %) a) ms)))
   :description
   (string/join ", and " (map :description ms))})

(defn any-of
  "passes if any of the given matchers pass:

  (matcha/run-match (matcha/any-of (matcha/= 1) (matcha/= 2)) 2) ; => passes
  (matcha/run-match (matcha/any-of (matcha/= 3) (matcha/= 2)) 1) ; => fails"
  [& ms]
  {:match
   (fn [a]
     (reduce (fn [x y] (or x y))
             (map #((:match %) a) ms)))

   :description
   (string/join ", or " (map :description ms))})

(defn has-count
  "passes if the sequence received has the given count
  (matcha/run-match (matcha/has-count 1) [1]) ; => passes
  (matcha/run-match (matcha/has-count 2) [])  ; => fails"
  [n]
  (on count (= n) "count" "a collection"))

(defn has-nth
  "passes if the sequence received has the value matching the matcher given at
  (nth n)

  (matcha/run-match (matcha/has-count 1) [1]) ; => passes
  (matcha/run-match (matcha/has-count 2) [])  ; => fails"
  [m n]
  (on #(nth % n) m "nth value" "a collection"))

(defn includes
  "passes if the sequence received includes the given item

  (matcha/run-match (matcha/includes 1) [1]) ; => passes
  (matcha/run-match (matcha/includes 1) [2]) ; => fails"
  [x]
  {:match
   (fn [xs] (core/some #{x} xs))
   :description
   (str "includes " (pr-str x))
   :describe-mismatch
   standard-describe-mismatch})

(defn not
  "passes if the given matcher fails
  (matcha/run-match (matcha/not (matcha/= 1)) 1) ; => passes
  (matcha/run-match (matcha/not (matcha/= 2)) 1) ; => fails"
  [m]
  {:match #(core/not ((:match m) %))
   :description (str "not " (:description m))
   :describe-mismatch
   standard-describe-mismatch})

(defn describe-class-mismatch [x]
  (str "was " (pr-str x)
       (if (clojure.core/nil? x)
         ""
         (str " <" (class x) ">"))))

(defn instance?
  "passes if the value matches the given class
  (matcha/run-match (matcha/instance? clojure.lang.Keyword) :foo) ; => passes
  (matcha/run-match (matcha/instance? clojure.lang.Keyword) 1) ; => fails"
  [klazz]
  {:match #(core/instance? klazz %)
   :description (str "an instance of " (.getName ^java.lang.Class klazz))})

(defn isa?
  "passes if the value isa? the given object

  (matcha/run-match (matcha/isa? :parent) :child) ; => passes
  (matcha/run-match (matcha/isa? :child) :child) ; => fails"
  [parent]
  {:match #(core/isa? % parent)
   :description (str "something with a parent of " parent)})

(def
  ^{:doc
    "passes if the value is a string
    (matcha/run-match matcha/string? \"foo\") ; => passes
    (matcha/run-match matcha/string? 1) ; => fails"}
  string?
  {:match core/string?
   :description "a string"})

(def
  ^{:doc
    "passes if the value is a map
    (matcha/run-match matcha/map? {}) ; => passes
    (matcha/run-match matcha/map? 1) ; => fails"}
  map?
  {:match core/map?
   :description "a map"})

(def
  ^{:doc
    "passes if the value is a seq
    (matcha/run-match matcha/seq? {}) ; => passes
    (matcha/run-match matcha/seq? 1) ; => fails"}
  seq?
  {:match core/seq?
   :description "a sequence"})

(def
  ^{:doc
    "passes if the value is a char
    (matcha/run-match matcha/char? {}) ; => passes
    (matcha/run-match matcha/char? 1) ; => fails"}
  char?
  {:match core/char?
   :description "a character"})

(def
  ^{:doc
    "passes if the value is a vector
    (matcha/run-match matcha/vector? {}) ; => passes
    (matcha/run-match matcha/vector? 1) ; => fails"}
  vector?
  {:match core/vector?
   :description "a vector"})

(def
  ^{:doc
    "passes if the value is a keyword
    (matcha/run-match matcha/keyword? :foo) ; => passes
    (matcha/run-match matcha/keyword? 1) ; => fails"}
  keyword?
  {:match core/keyword?
   :description "a keyword"})

(def
  ^{:doc
    "passes if the value is a symbol
    (matcha/run-match matcha/symbol? 'foo) ; => passes
    (matcha/run-match matcha/symbol? 1) ; => fails"}
  symbol?
  {:match core/symbol?
   :description "a symbol"})

(def
  ^{:doc
    "passes if the value is nil
    (matcha/run-match matcha/nil? nil) ; => passes
    (matcha/run-match matcha/nil? 1) ; => fails"}
  nil?
  {:match core/nil?
   :description "nil"})

(def
  ^{:doc
    "passes if the value is a ratio

    (matcha/run-match matcha/ratio? 1/5) ; => passes
    (matcha/run-match matcha/ratio? 5) ; => fails"}
  ratio?
  {:match core/ratio?
   :description "a ratio"})

(def
  ^{:doc
    "passes if the value is a decimal

    (matcha/run-match matcha/decimal? bigdec 1)) ; => passes
    (matcha/run-match matcha/decimal? 5) ; => fails"}
  decimal?
  {:match core/decimal?
   :description "a decimal"})

(def
  ^{:doc
    "passes if the value is a rational

    (matcha/run-match matcha/rational? 1) ; => passes
    (matcha/run-match matcha/rational? \"a\") ; => fails"}
  rational?
  {:match core/rational?
   :description "a rational"})

(def
  ^{:doc
    "passes if the value is a float

    (matcha/run-match matcha/float? (float 1.0)) ; => passes
    (matcha/run-match matcha/float? 5) ; => fails"}
  float?
  {:match core/float?
   :description "a float"})

(def
  ^{:doc
    "passes if the value is a collection

    (matcha/run-match matcha/coll? []) ; => passes
    (matcha/run-match matcha/coll? 5) ; => fails"}
  coll?
  {:match core/coll?
   :description "a collection"})

(def
  ^{:doc
    "passes if the value is a list

    (matcha/run-match matcha/list? '()) ; => passes
    (matcha/run-match matcha/list? 5) ; => fails"}
  list?
  {:match core/list?
   :description "a list"})

(def
  ^{:doc
    "passes if the value is a set

    (matcha/run-match matcha/set? #{}) ; => passes
    (matcha/run-match matcha/set? 5) ; => fails"}
  set?
  {:match core/set?
   :description "a set"})

(defn assert-good-matcher [{:keys [match description describe-mismatch] :as matcher}]
  (assert (not (nil? matcher)) "Matcher should not be nil")
  (assert match
          (str "matcher should have a :match key. Matcher: " matcher))
  (assert description
          (str "matcher should have a :description key. Matcher: " matcher))

  (assert (core/= 0 (count (dissoc matcher :match :description :describe-mismatch)))
          (str "matcher shouldn't have extra keys, but had "
               (describe-list "" (dissoc matcher :match :description :describe-mismatch)))))

(defn run-match
  "runs a matcher, given a value to match against.
  Returns a map:

  if the matcher matches the value:
  {:pass? true}

  if the matcher fails:
  {:pass? false
   :matcher matcher
   :expected (a string)
   :was (a string)}

  the results can be made human readable with (format-message result)"
  [matcher a]
  (assert-good-matcher matcher)
  (if ((:match matcher) a)
    {:pass? true}
    {:pass? false
     :expected (:description matcher)
     :was ((:describe-mismatch matcher describe-class-mismatch) a)
     :matcher matcher}))
