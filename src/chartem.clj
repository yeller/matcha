(ns chartem
  (:refer-clojure :exclude [empty? every? = some not <= >=])
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
;;  WIP:
;;  TODO: matchers list
;; -- ** Matchers on seqs
;; -- ** Matchers on numbers
;; -- ** Matchers on nil
;; , nil?
;; -- ** Matcher combinators
;; , is-not
;; , any-of
;; , on
;; , and-also
;; -- ** Utility functions for writing your own matchers
;; , matcher-on
;; , match-list
(defn describe-list [call xs]
  (str "(" call " " (clojure.string/join " " xs) ")"))

(defn standard-describe-mismatch [x]
  (str "was " (pr-str x)))

(defn =
  "matches based on equality of the value given

  (chartem/run-match (chartem/= 1) 1) ; => passes
  (chartem/run-match (chartem/= 1) 2) ; => fails"
  [a]
  {:match (fn [b] (core/= a b))
   :description (describe-list "=" [a])
   :describe-mismatch standard-describe-mismatch})

(defn <=
  "matches based if the value given is greater-than or equal to

  (chartem/run-match (chartem/<= 1) 1) ; => passes
  (chartem/run-match (chartem/<= 1) 0) ; => fails"
  [a]
  {:match (fn [b] (core/<= a b))
   :description (describe-list "<=" [a])
   :describe-mismatch standard-describe-mismatch})

(defn <
  "matches based if the value given is greater-than or equal to

  (chartem/run-match (chartem/< 1) 2) ; => passes
  (chartem/run-match (chartem/< 1) 0) ; => fails"
  [a]
  {:match (fn [b] (core/< a b))
   :description (describe-list "<" [a])
   :describe-mismatch standard-describe-mismatch})

(defn >
  "matches based if the value given is greater-than or equal to

  (chartem/run-match (chartem/> 1) 2) ; => passes
  (chartem/run-match (chartem/> 1) 0) ; => fails"
  [a]
  {:match (fn [b] (core/> a b))
   :description (describe-list ">" [a])
   :describe-mismatch standard-describe-mismatch})

(defn >=
  "matches based if the value given is less-than or equal to

  (chartem/run-match (chartem/<= 1) 1) ; => passes
  (chartem/run-match (chartem/<= 1) 0) ; => fails"
  [a]
  {:match (fn [b] (core/>= a b))
   :description (describe-list ">=" [a])
   :describe-mismatch standard-describe-mismatch})

(def empty?
  "matches if the collection passed is empty

  (chartem/run-match chartem/empty? [1]) ; => passes
  (chartem/run-match chartem/empty? [])  ; => fails"
  {:match core/empty?
   :description "(empty?)"
   :describe-mismatch standard-describe-mismatch})

(defn format-message
  "turns the results of a failing match into a human readable error message,
   suitable for printing with clojure.core/print or clojure.core/println"
  [result]
  (str "\nExpected: " (:expected result)
       "\n     but: "
       (:was result)))

(defn all-of
  "matches if all of the matchers given pass:

  (chartem/run-match (chartem/all-of (chartem/= 1) (chartem/= 1)) 1) ; => passes
  (chartem/run-match (chartem/all-of (chartem/= 1) (chartem/= 1)) 2) ; => fails"
  [& ms]
  {:match
   (fn [a] (reduce (fn [x y] (and x y))
                          (map #((:match %) a) ms)))
   :description
   (describe-list "all-of" (map :description ms))
   :describe-mismatch
   standard-describe-mismatch})

(defn any-of
  "passes if any of the given matchers pass:

  (chartem/run-match (chartem/any-of (chartem/= 1) (chartem/= 2)) 2) ; => passes
  (chartem/run-match (chartem/any-of (chartem/= 3) (chartem/= 2)) 1) ; => fails"
  [& ms]
  {:match
   (fn [a]
     (reduce (fn [x y] (or x y))
             (map #((:match %) a) ms)))

   :description
   (describe-list "any-of" (map :description ms))

   :describe-mismatch
   standard-describe-mismatch})

(defn some
  [m]
  "passes if the matcher given passes on any of the items of the sequence this matcher receives
  (chartem/run-match (chartem/some (chartem/= 1)) [1]) ; => passes
  (chartem/run-match (chartem/some (chartem/= 0)) [1]) ; => fails"
  {:match
   (fn [xs] (core/some #((:match m) %) xs))
   :description
   (describe-list "some" [(:description m)])
   :describe-mismatch
   standard-describe-mismatch})

(defn every?
  [m]
  "passes if the matcher given passes against any of the items of the sequence the resulting matcher receives"
  {:match
   (fn [xs] (core/every? #((:match m) %) xs))
   :description
   (describe-list "every?" [(:description m)])
   :describe-mismatch
   standard-describe-mismatch})

(defn has-count
  "passes if the sequence received has the given count
  (chartem/run-match (chartem/has-count 1) [1]) ; => passes
  (chartem/run-match (chartem/has-count 2) [])  ; => fails"
  [n]
  {:match (fn [xs] (clojure.core/= (count xs) n))
   :description (describe-list "has-count" [n])
   :describe-mismatch
   standard-describe-mismatch})

(defn includes
  "passes if the sequence received includes the given item

  (chartem/run-match (chartem/includes 1) [1]) ; => passes
  (chartem/run-match (chartem/includes 1) [2]) ; => fails"
  [x]
  {:match
   (fn [xs] (core/some #{x} xs))
   :description
   (describe-list "includes" [x])
   :describe-mismatch
   standard-describe-mismatch})

(defn not
  "passes if the given matcher fails
  (chartem/run-match (chartem/not (chartem/= 1)) 1) ; => passes
  (chartem/run-match (chartem/not (chartem/= 2)) 1) ; => fails"
  [m]
  {:match #(core/not ((:match m) %))
   :description (describe-list "not" [m])
   :describe-mismatch
   standard-describe-mismatch})

(defn assert-good-matcher [{:keys [match description describe-mismatch] :as matcher}]
  (assert (not (nil? matcher)) "Matcher should not be nil")
  (assert match
          (str "matcher should have a :match key. Matcher: " matcher))
  (assert description
          (str "matcher should have a :description key. Matcher: " matcher))
  (assert describe-mismatch
          (str "matcher should have a :describe-mismatch key. Matcher: " matcher))

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
  :expected (a string)
  :was (a string)}

  the results can be made human readable with (format-message result)"
  [matcher a]
  (assert-good-matcher matcher)
  (if ((:match matcher) a)
    {:pass? true}
    {:pass? false
     :expected (:description matcher)
     :was ((:describe-mismatch matcher) a)}))
