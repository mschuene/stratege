Stratege
========

A clojure library for strategic term rewriting.

```clojure
(require '[stratege.core :as s :refer [defrule rules]])

(defrule defi [:impl x y] -> [:or [:not x] y])
(defrule defe [:eq x y] -> [:and [:impl x y] [:impl y x]])

(defrule dn    [:not [:not x]] -> x)

(defrule dma   [:not [:and x y]] -> [:or [:not x] [:not y]])
(defrule dmo   [:not [:or x y]] -> [:and [:not x] [:not y]])

(defrule daol  [:and [:or x y] z] -> [:or [:and x z] [:and y z]])
(defrule daor  [:and z [:or x y]] -> [:or [:and z x] [:and z y]])

(defrule doal  [:or [:and x y] z] -> [:and [:or x z] [:or y z]])
(defrule doar  [:or z [:and x y]] -> [:and [:or z x] [:or z y]])

(def dnf (s/innermost (rules dn defi defe dma dmo daol daor)))
(def cnf (s/innermost (rules dn defi defe dma dmo doal doar)))

(is (= (dnf [:not [:or [:and [:not :x] :y] :z]])
       [:or [:and :x [:not :z]] [:and [:not :y] [:not :z]]]))

(is (= (cnf [:not [:or [:and [:not :x] :y] :z]])
       [:and
        [:and [:or :x [:not :y]] [:or :x [:not :z]]]
        [:and [:or [:not :z] [:not :y]] [:or [:not :z] [:not :z]]]]))
```
