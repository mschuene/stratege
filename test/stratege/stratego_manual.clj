(ns stratege.stratego-manual
  (:require  [clojure.test :refer :all]))





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

(def dnf (innermost (ruleset dn def-i def-e dma dmo daol daor)))
(def cnf (innermost (ruleset dn defn-i def-e dma dmo doal doar)))


(deftest test-cnf-dnf
  (is (= '[:and [:and [:or [:not true] [:not false]] [:or [:not true] [:atom x]]]
          [:and [:or [:atom x] [:not false]] [:or [:atom x] [:atom x]]]]
         (cnf [:not [:or [:and true false] [:not [:atom 'x]]]])))
  (is (= '[:or [:and [:not true] [:atom x]] [:and [:not false] [:atom x]]]
         (dnf [:not [:or [:and true false] [:not [:atom 'x]]]]))))


(deftest test-con
  (is (= [2 1 3] ((con :vector (simple inc) (simple dec) id) [1 2 3]))))
