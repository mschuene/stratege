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

There are some libraries like kibit, termito and expresso which define
a term rewriting system, but on top of core.logic and they are prone
to stack overflows on deeply nested terms. Indeed, part of my
motivation to write stratege stem from the desire to have a better
core term rewriting system for expresso.

Stratege is based on the following key ideas:
- operate on terms through zippers.
- separate rules from strategies and expose composeable strategy combinators to define
  the traversals like StrategoXT does.
- strategies are internally written in a continuation passing style so that they don't
  blow up the stack.
- use core.match for rule definitions.


### Getting started

Add the following line to your leiningen dependencies: (not jet released!)
```clojure
[stratege "0.1.0"]
```

### Rules and strategies

In the example given above, rules are constructed and given a name
with the defrule macro from stratege.core. (defrule name rule-args) is
just a shortcut for (def name (rule rule-args)).
In the simple case, rule takes 3 arguments, the left-hand-side pattern, some thing in the middle, and the right-hand-side pattern and acts on the current node. The patterns use normal core.match syntax.

A rule can be called as a function or combined with other rules in a (rules ...) form.

```clojure
(is (= (defi [:impl :x :y]) [:or [:not :x] :y]))
```
In a (rules ...) form all arguments must be either calls to the rule (or strategic-rule explained later) macro or vars pointing to a defined rule. All the rules in a (rules ...) form are compiled to a clojure.core.match/match call. This way, the matching rule can be determined by a single sweep through the expression instead of a sweep per rule.

If you don't want to name the rules and reuse them elsewhere, then the match-replace macro provides a more direct resemblance to the pattern matching that is going on.
The next example shows how to evaluate a simple logical formula.

```clojure
(require '[stratege.core :as s])
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
```

Rules are just special cases of strategies. While with the Rules you specify what you are transforming, the strategies specify how the term is walked and where and how often the rules are applied.

One example of a strategy is the innermost strategy used in the first
example. It exhaustively applies the given strategy in a bottomup
fashion until a normal form is reached.

Innermost application is sometimes too much. Part of the rationale of
using strategies and strategy combinators is having finer control
about rule application. In the case of propagate-constants-rules, a single bottomup traversal of the term would be enough. However, it may not be the most efficient. With the
[:and false x] rule, a possible large subterm x doesn't need to be traversed at all. Therefore, a single downup traversal is more adequate.

```clojure
(def propagate-constants (s/downup propagate-constants-rules)
(is (= true (propagate-constants [:and true [:or true false]])))
```

The following is a list of the strategies provided by stratege.

### higher level traversal strategies

#### bottomup

#### topdown

#### downup

#### downup2

#### leavestd and leavesbu

#### onebu and onetd

#### somebu and sometd

#### innermost

### Core strategies and strategy transformations

#### id

#### fail

#### <+

#### <*

#### all, some and one

#### attempt

#### repeat

#### on-node and on-local

#### replace

#### where and guard

#### scope

#### emit-bindings

#### debug

#### strategic-match and match-replace

#### strategic-rule and rule

