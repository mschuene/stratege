(ns stratege.stratego-manual
  (:require  [clojure.test :refer :all]
             [stratege.core :as s :refer [defrule defrules rule]]))





;;; test rules example from stratego manual dnf to cnf
;;; expressions are of the form true | false | [:atom "name"] | [:not x] | [:and x y] | [:or x y] | [:impl x y] | [:eq x y]

(defrule def-i [:impl x y] -> [:or [:not x] y])
(defrule def-e [:eq x y] -> [:and [:impl x y] [:impl y x]])

(defrule dn    [:not [:not x]] -> x)

(defrule dma   [:not [:and x y]] -> [:or [:not x] [:not y]])
(defrule dmo   [:not [:or x y]] -> [:and [:not x] [:not y]])

(defrule daol  [:and [:or x y] z] -> [:or [:and x z] [:and y z]])
(defrule daor  [:and z [:or x y]] -> [:or [:and z x] [:and z y]])

(defrule doal  [:or [:and x y] z] -> [:and [:or x z] [:or y z]])
(defrule doar  [:or z [:and x y]] -> [:and [:or z x] [:or z y]])


(deftest test-defrule
  (is (= (def-i [:impl true false]) [:or [:not true] false])))

(def dnf (s/innermost (s/rules dn def-i def-e dma dmo daol daor)))
(def cnf (s/innermost (s/rules dn def-i def-e dma dmo doal doar)))


(deftest test-cnf-dnf
  (is (= '[:and [:and [:or [:not true] [:not false]] [:or [:not true] [:atom x]]]
          [:and [:or [:atom x] [:not false]] [:or [:atom x] [:atom x]]]]
         (cnf [:not [:or [:and true false] [:not [:atom 'x]]]])))
  (is (= '[:or [:and [:not true] [:atom x]] [:and [:not false] [:atom x]]]
         (dnf [:not [:or [:and true false] [:not [:atom 'x]]]]))))



(def propagate-constants-rules
  (s/match-replace
   [:and true x] x
   [:and x true] x
   [:and false x] false
   [:and x false] false
   [:or true x] true
   [:or x true] true
   [:or false x] x
   [:or x false] x
   [:not false] true
   [:not true] false))


(is (= [:or true false] (propagate-constants-rules [:and true [:or true false]])))

(def propagate-constants (s/downup (s/attempt propagate-constants-rules)))


(is (= true (propagate-constants [:and true [:or true false]])))
