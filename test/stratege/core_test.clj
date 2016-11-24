(ns stratege.core-test
  (:require [clojure.test :refer :all]
            [criterium.core :refer [bench with-progress-reporting]]
            [stratege.core :as s]))

(def test-strategy
  (s/attempt (s/guard sequential?
                  (s/on-node (fn [[{:keys [n] :or {n 1} :as b} t]]
                               [(update b :n (fnil inc 1)) (into [] (concat t [n]))])))))

(deftest test-id
  (is (= (s/id 1) 1)))

(deftest test-fail
  (is (= (s/fail 1) nil)))

(deftest test-<*
  (is (= 4 ((s/<* (s/replace {1 2}) (s/replace {2 4})) 1)))
  (is (nil? ((s/<* s/id s/fail) 1)))
  (is (nil? ((s/<* s/fail s/id) 1))))

(deftest test-<+
  (is (= 2 ((s/<+ (s/replace inc) (s/replace inc)) 1)))
  (is (= 2 ((s/<+ s/fail (s/replace inc)) 1)))
  (is (nil? ((s/<+ s/fail s/fail) 1))))

(deftest test-negation
  (is (nil? ((s/negation s/id) 1)))
  (is (= 1 ((s/negation s/fail) 1))))

(deftest test-all
  (is (= [2 2] ((s/all (s/replace {1 2})) [1 1])))
  (is (nil? ((s/all (s/replace {1 2})) [1 2])))
  (is (= 1 ((s/all s/fail) 1))))

(deftest test-one
  (is (= [2 1] ((s/one (s/replace {1 2})) [1 1])))
  (is (= [2 2] ((s/one (s/replace {1 2})) [1 2])))
  (is (nil? ((s/one s/id) 1))))

(deftest test-some
  (is (= [2 2] ((s/some (s/replace {1 2})) [1 1])))
  (is (= [2 2] ((s/some (s/replace {1 2})) [1 2])))
  (is (nil? ((s/some s/id) 1))))

(deftest test-attempt
  (is (= 1 ((s/attempt s/fail) 1)))
  (is (= 2 ((s/attempt (s/replace {1 2})) 1))))

(deftest test-repeat
  (is (= 5 ((s/repeat (s/replace {1 2 2 3 3 4 4 5})) 1))))

(deftest test-topdown
  (is (= [[[[[1 5] 4] 3] 2] 1]
         ((s/topdown test-strategy) (nth (iterate vector 1) 5)))))

(deftest test-bottomup
  (is (= [[[[[1 1] 2] 3] 4] 5]
         ((s/bottomup test-strategy) (nth (iterate vector 1) 5)))))

(deftest test-downup
  (is (= [[[[[1 5 6] 4 7] 3 8] 2 9] 1 10]
         ((s/downup test-strategy) (nth (iterate vector 1) 5)))))

(deftest test-onebu
  (is (= [1 "replaced" [[1]] [[[1]]] [[[[1]]]]]
         ((s/onebu (s/guard sequential? (s/replace (constantly "replaced"))))
                         (apply vector (take 5 (iterate vector 1)))))))

(deftest test-somebu
  (is (= [1 "replaced" ["replaced"] [["replaced"]] [[["replaced"]]]]
         ((s/somebu (s/guard sequential? (s/replace (constantly "replaced"))))
          (apply vector (take 5 (iterate vector 1)))))))

(deftest test-downup2
  (is (= [[[[[[[[[[[1] [5]]] [4]]] [3]]] [2]]] [1]]]
         ((s/downup2 test-strategy (s/replace vector)) (nth (iterate vector 1) 5)))))

(deftest test-alltd
  (is (= [2 [2 2 2]] ((s/alltd (s/replace {1 2})) [2 [1 1 1]]))))

(deftest test-onetd
  (is (= [2 [3 "replaced" [2 1]]]
         ((s/onetd (s/replace {1 "replaced"})) [2 [3 1 [2 1]]]))))

(deftest test-sometd
  (is (= [2 [3 "replaced" [2 "replaced"]]]
         ((s/sometd (s/replace {1 "replaced"})) [2 [3 1 [2 1]]]))))

(deftest test-?-!
  (is (= 1 ((s/<* (s/? '?a) (s/! '?a)) 1)))
  (is (nil? ((s/<* (s/? '?a) (s/! 2) (s/? '?a)) 1)))
  (is (= 1 ((s/? '?a) 1)))
  (is (= 2 ((s/! 2) 1))))

(deftest test-scope
  (is (= '?a ((s/<* (s/scope '[?a] (s/? '?a)) (s/! '?a)) 1))))


(deftest test-where
  (is (= 1 ((s/<* (s/where (s/<* (s/? '?a) (s/! 2))) (s/! '?a)) 1))))

(deftest test-replace
  (is (= 2 ((s/replace {1 2}) 1))))

(deftest test-guard
  (is (nil? ((s/guard sequential? s/id) 1)))
  (is (= [1] ((s/guard sequential? s/id) [1]))))

(deftest test-emit-bindings
  (is (= {'?a 1} (first ((s/<* (s/? '?a) (s/emit-bindings ['?a])) 1)))))

(deftest test-match-replace
  (is (= [2 1] ((s/match-replace [+ ?a ?b] [?b ?a]) ['+ 1 2]))))

(deftest test-strategic-match
  (is (= [{:a 1, :b 2} ['+ 1 2]]
         ((s/<* (s/strategic-match [+ ?a ?b] (s/put-bindings {:a ?a :b ?b}))
              (s/emit-bindings [:a :b]))
          ['+ 1 2]))))


(deftest test-leaves
  (is (= ((-> (fn [node] (when (and (integer? node) (< node 3)) [(inc node) "hi"]))
              s/replace s/attempt s/leaves) [1])
         [[2 "hi"]])))


(def r1 (s/rule false -> false))
(deftest test-false-is-not-nil
  (= false ((s/rules r1) false)))



