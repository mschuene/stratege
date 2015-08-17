(ns stratege.match-test
  (:require [clojure.core.match
             :refer [match to-source emit-pattern-for-syntax emit-pattern]]))


(defrecord RecordMatcher [pattern])

(deftype RecordMatcherType [pattern])

(match [[2 3]]
       [(a :<< first) :as loc])
