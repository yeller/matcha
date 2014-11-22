(ns chartem)
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
;; -- ** Matchers on seqs
;; , is-empty
;; , has-size
;; , every-item
;; , include?
;; -- ** Matchers on numbers
;; , <
;; , <=
;; , >
;; , >=
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

(defn =
  "matches based on equality of the value given

  (chartem/run-match (chartem/= 1) 1) ; => passes
  (chartem/run-match (chartem/= 1) 2) ; => fails "
  [a]
  {:match (fn [b] (= a b))
   :description (str "(= " (pr-str a) ")")
   :describe-mismatch (fn [b] (pr-str b))})

(defn format-message
  "turns the results of a failing match into a human readable error message,
   suitable for printing with clojure.core/print or clojure.core/println"
  [result]
  (str "\nExpected: " (:expected result)
       "\n     but: "
       (:was result)))

(defn describe-list [call xs]
  (str "(" call " " (clojure.string/join " " xs) ")"))

(defn every?
  "matches if all of the matchers given pass:

  (chartem/run-match (every? (chartem/= 1) (chartem/= 1)) 1) ; => passes
  (chartem/run-match (every? (chartem/= 1) (chartem/= 1)) 2) ; => fails"
  [ms]
  {:match
   (fn [a] (reduce (fn [x y] (and x y))
                          (map #((:match %) a) ms)))
   :description
   :describe-mismatch
   (describe-list "every?" (map :description ms))
   (fn [a]
     (->> ms
       (filter #(not ((:match %) a)))
       (map #((:describe-mismatch %) a))))})

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
  (if ((:match matcher) a)
    {:pass? true}
    {:pass? false
     :expected (:description matcher)
     :was ((:describe-mismatch matcher) a)}))
