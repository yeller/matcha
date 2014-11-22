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

(defn equals [a]
  {:match (fn [b] (= a b))
   :description (str "(= " (pr-str a) ")")
   :describe-mismatch (fn [b] (pr-str b))})

(defn format-message [result]
  (str "\nExpected: " (:expected result)
       "\n     but: "
       (:was result)))

(defn every?
  [ms]
  {:match (fn [a] (reduce (fn [x y] (and x y))
                          (map #((:match %) a) ms)))
   :description (str "(every? " (clojure.string/join " "
                                                     (map :description ms)) ")")
   :describe-mismatch })

(defn run-match
  [matcher a]
  (if ((:match matcher) a)
    {:pass? true}
    {:pass? false
     :expected (:description matcher)
     :was ((:describe-mismatch matcher) a)}))
