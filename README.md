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

(is (= (dnf [:or [:not [:or [:not :x] :y]] :z])
       [:or [:and :x [:not :y]] :z]))

(is (= (cnf [:or [:not [:or [:not :x] :y]] :z])
       [:and [:or :x :z] [:or [:not :y] :z]]))
```

### Rationale

Many data transformations can be expressed by term rewriting in a
natural way. There are already great libraries for matrix programming,
logic programming, async programming, etc. for clojure but term
rewriting seems to be a gap in the clojure ecosystem.

There are some libraries like kibit, termito and expresso which define a term rewriting system, but on top of core.logic and they are prone to stack overflows on deeply nested terms. Indeed, part of my motivation to write stratege stem from the desire to have a better core term rewriting system for expresso.

Stratege is based on the following key ideas:
- operate on terms through zippers.
- separate rules from strategies and expose composeable strategy combinators to define
  the traversals like StrategoXT does.
- strategies are internally written in a continuation passing style so that they don't
  blow up the stack.
- use core.match for rule definitions.


