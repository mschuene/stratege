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
natural way. There are already great clojure libraries for matrix
programming, logic programming, async programming, etc. but term
rewriting seems to be a gap in the clojure ecosystem.

There are already libraries like kibit, termito and expresso which
define a term rewriting system, but they do that on top of core.logic
and are prone to stack overflows on deeply nested terms. Indeed,
part of my motivation to write stratege stems from the desire to have a
better core term rewriting system and rewrite expresso to use it.

Stratege is based on the following key ideas:
- Operate on terms through zippers.
- Separate rules from strategies and expose composeable strategy combinators to define
  the traversals like StrategoXT does.
- Strategies are internally written in a continuation passing style so that they don't
  blow up the stack.
- core.match is used for rule definitions.


### Getting started

Add the following line to your leiningen dependencies:

```clojure
[stratege "0.1.0"]
```

### Rules and strategies

In the example given above, rules are constructed and given a name
with the defrule macro from stratege.core. (defrule name rule-args) is
just a shortcut for (def name (rule rule-args)). In the simple case,
rule takes 3 arguments, the left-hand-side pattern, the ->, and the
right-hand-side pattern and acts on the current node. The patterns use
normal core.match syntax.

A rule can be called as a function or combined with other rules in a
(rules ...) form.

```clojure
(is (= (defi [:impl :x :y]) [:or [:not :x] :y]))
```

In a (rules ...) form, all arguments must be either calls to the rule
macro (or strategic-rule macro explained in the section matching and
rules in depth) or vars pointing to a defined rule. All the rules in a
(rules ...) form are compiled to a single clojure.core.match/match
call. This way, the matching rule can be determined by a single sweep
through the expression instead of a sweep per rule.

If you don't want to name the rules and reuse them elsewhere, then the
match-replace macro provides a more direct resemblance to the pattern
matching that is going on. The next example shows how to evaluate a
simple logical formula.

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

Rules are just special cases of strategies. While with the Rules you
specify what you are transforming, the strategies specify how the term
is walked and where and how often the rules are applied.

One example of a strategy is the innermost strategy used in the first
example. It exhaustively applies the given strategy in a bottomup
fashion until a it can't be applied anymore.

Innermost application is sometimes too much. Part of the rationale of
using strategies and strategy combinators is having finer control
about rule application. In the case of propagate-constants-rules, a
single bottomup traversal of the term would be enough. However, it may
not be the most efficient. With the [:and false x] rule, a possible
large subterm x doesn't need to be traversed at all. Therefore, a
single downup traversal is more adequate.

```clojure
(def propagate-constants (s/downup propagate-constants-rules)
(is (= true (propagate-constants [:and true [:or true false]])))
```

While strategies can be called like rules on a term and return a term
or nil when they failed, internally the strategies get and return a
pair of
- a binding map and
- the current zipper location.
Such a pair is called state in the remainder of this overview.

The bindings map provides means of passing along information during
the traversal, for example defined variables when doing contextual
analysis of a program. It also stores the currently used zipper functions.

Operating on the zipper instead of the term gives the strategies the
ability to move around the location and also to inspect the terms
surrounding context.

Therefore, the two main problems of term rewriting can be lessened
with stratege. Composeable strategies relieve the intertwinging of
rule application and traversal definitions and operating on zippers
gives access to the surrounding context when needed.

Additionally, strategies in stratege are implemented in a continuation
passing style, which allows recursive strategy definitions without
risking stack overflow on big terms. This enables the use of core strategy
combinators to define high level strategies like bottomup etc.

#### Calling strategies as functions

Strategies are implemented as a custom type that has a combine method
with the cps interface for internal use and also implements IFn for
easier invokation by client code.

When you invoke a strategy as a function, it takes a term and returns
the modified term or nil in case of failure.

You can specify on optional second argument, that indicates the
starting bindings. For example, you can tell the strategy to use the
seq-zip instead of the default vector-zip by calling it with

```clojure
(s term {:zip-make-zip zip/seq-zip})
```

### Core strategies and strategy transformations

#### id

The id strategy always succeeds and returns the given bindings and location.

#### fail

The fail strategy always fails and constantly returns nil.

#### <+

<+ is the deterministic choice strategy combinator (<+ s1 .... sn)
tries to apply s1 and succeeds with the returned state from s1 if it
succeeds. If s1 fails, it tries to apply s2 .... until sn. It fails if
all strategies failed.

#### <*

<* is the sequential composition of strategies. (<* s1 .. sn) tries to
apply first s1 and if it succeeds, applies s2 with the state returned
by s1 and so on until sn succeeds and returns its resulting state.
If any of s1 ... sn fails, <* fails.

#### attempt

The attempt strategy combinator tries to apply the given strategy,
returns its resulting state if it succeeds, but returnes the
unmodified initial state if the strategy fails.

```clojure
(defn attempt [s] (<+ s id))
```
#### repeat

repeat is the first example of a recursive strategy combinator. It
applies the given strategy exhaustively until it fails, returning the
resulting state from the last successful application.

```clojure
(defn repeat [s] (attempt (<* s (rec repeat s))))
```

rec is a utility function to make recursive strategy definitions more
succinct. It creates an anonymous strategy that applies the given
strategy to the given terms when its called.

#### all, some and one

To be able to define strategies that act upon subterms, the all, some
and one combinators can be used.

(all s) applies s to all subterms left to right passing the created
bindings and succeeds when s succeeded on all subterms. Fails if one
application of s fails. Acts like id on leaves.

(one s) and (some s) try to apply the given stratego to one or to at
most subterms as possible. Both fail when no application of s failed
or when the current location is a leave.

### higher level traversal strategies

With the strategy combinators introduced so far, the more usual
traversal strategies can be explained.

#### bottomup

The bottomup strategy combinator applies s in a bottom up fashion,
starting with the leaves. Succeeds if and only if all applications of
s succeeded.

```clojure
(defn bottomup [s] (<* (all (rec bottomup s)) s))
```

#### topdown

The topdown strategy combinator applies s in a top down
fashing, starting with the current loc and working its way down to the
leaves. Succeeds iff all applications of s succeeded.

```clojure
(defn topdown [s] (<* s (all (rec topdown s))))
```

#### downup and downup2

The downup strategy combinator applies s on the descend as well as on
the ascend. Succeeds iff all applications of s succeeded.

```clojure
(defn downup [s] (<* s (all (rec downup s)) s))
```

downup2 is like downup, but it takes two strategies s1 and s2 as
arguments and applies s1 on the descend and s2 on the ascend.

#### onebu and onetd

The onebu and onetd combinators are versions of the bottomup and
topdown combinators that use one instead of all, therefore trying to
apply the strategy to one subterm in a bottomup or topdown manner.

```clojure
(defn onebu [s] (<+ (one (rec onebu s)) s))
(defn onetd [s] (<+ s (one (rec onetd s))))
```
#### somebu and sometd

The somebu and sometd combinators are versions of the bottomup and
topdown combinators that use some instead of all, therefore trying to
apply the strategy to at most subterms as possible in a bottomup or
topdown manner.

```clojure
(defn somebu [s] (<+ (some (rec somebu s)) s))
(defn sometd [s] (<+ s (some (rec sometd s))))
```

#### innermost

The innermost strategy combinator applies s repeatedly bottomup until
it can't be applied anymore.

```clojure
(defn innermost [s] (bottomup (attempt (<* s (rec innermost s)))))
```

#### leaves

The leaves strategy combinator acts like bottomup but only applies s
when the current location is a leave.

### Other strategy combinators

#### where

where applies s, but restores the original location when s succeeds
but keeps the bindings made by s.

#### on-node and on-loc

on-node and on-loc are helpers to define strategies from non cps
functions operating on a pair of [bindings node] or
[bindings location] respectively.

#### replace

replace turns a function operating on the current node to a strategy
that performs this operation and replaces the current location with
the return value of the function. Thus

```clojure
((bottomup (attempt (replace {1 2}))) term)
```

is equivalent to using (clojure.walk/postwalk-replace {1 2} term) (when the zipper
used is appropriate).

### guard

guard only applies the strategy when the current node matches the
given predicate?.

```clojure
(defn guard [predicate? s] (<* (where (replace predicate?)) s))
```

#### scope

The (scope vars s) combinator limits the scope of the given vars to
the application of s, restoring the original vals for the vars after s
succeeded.


#### emit-bindings

(emit-bindings) replaces the current node by
[current-bindings current-node]. Useful when you want to retain the
bindings from one strategy invoked as a function. Takes an optional
vector of keys as argument and limits the exposed bindings to these
specified in the vector.

### put-bindings and update-bindings

```clojure
(put-bindings new-bindings)
```

put-bindings merges the new-bindings to the binding map. Always
succeeds. As does a update-bindings but it calls update on the binding
map with the given arguments.

#### debug

debug is an utility to debug the transformation process going on.
It always succeeds and does nothing to the state.
It takes either a single no-arg function that is called, or a vector indicating
the arguments to the function and a function taking the specified arguments.
Possible values in the vector are :node :loc or :bindings.

For example:

```clojure
((bottomup (debug [:node] prn)) [[2 3] 1])
```

prints

```clojure
2
3
[2 3]
1
[[2 3] 1]
[[2 3] 1]
```
### Matching and rules in depth

The beginning of this introduction gave examples of the rule, rules
and match-replace macros. These examples are showing the special case
where the current location is replaced by the instantiation of the
right hand side after the successful match of left hand side.

The general case allows you to specify the strategy executed after the left hand side has successfully been matched. The special case then comes down to
using (replace (constantly rhs)) as next strategy.

#### strategic-match and match-replace

match replace is the special case of strategic-match where the right
hand sides are wrapped in (replace (constantly rhs)). For example, you can use
strategic-match to put bindings in the binding map.

```clojure
(strategic-match [:let x = expr in y] (update-bindings :declared-vars conj x))
```

Both match-replace and strategic-match
take an options-map as an optional first argument. Currently, only the
option :match-on is supported. It's default value is :node which means
that the match is performed on the current node. Possible values are
:node, :loc, :bindings or a vector of these keywords. For example with


```clojure
(strategic-match {:match-on [:node :bindings :loc]}
    [node-pattern bindings-pattern loc-pattern] rhs-strategy)
```

you can match simultaneously on the current node, the current bindings
and bind the current location so you can inspect the context in a
:guard clause or in the right hand side.

One thing to notice is that the match is committed, once the left
hand side matches. When the application of the right hand side
strategy fails, other possible matches aren't considered.

Thus, while (match-replace lhs1 rhs1 lhs2 rhs2) is equivalent to, but
more performant as (<+ (match-replace lhs1 rhs1) (match-replace lhs2
rhs2)), this does not hold for strategic-match in case that the
strategies on the right hand side don't always succeed.

#### strategic-rule and rule

strategic-rule and rule have exactly the same relations as
strategic-match and match-replace. Both also take as optional first
argument an options-map with the same supported keys and rule is the
special case of strategic rule with wrapping the right hand side in
(replace (constantly rhs)).

The rules form accepts both rules and strategic-rules and the rules
can have different :match-on options.This way you can have most of
your rules context-free and matching only on :node and only the few
rules that need it have access to the bindings or location or can have
a strategy as right hand side.


### Other strategy combinators

There are other strategy combinators that are not mentioned in this
overview. Check the stratege.core namespace for them. 

### StrategoXT manual

The
[StrategoXT Manual](http://releases.strategoxt.org/strategoxt-manual/unstable/manual/chunk-book/index.html)
gives a nice introduction to strategic term rewriting and most of the
examples given in Chapters 12-17 can easily be translated to stratege.
The beginning example is a direct translation from an example in
section 13.5.1 of the manual. The manual also discusses some topics
addressed here in greater depth and I would suggest you to look into
it if you are interested in strategic term rewriting.

the ; in stratego corresponds to stratege's <* and try has been
renamed to attempt, the ? in stratego is more powerful than the ? in
stratege but uses of ? in stratego are more adequatly translated with
the strategic-match macro in stratege, there are no congruences yet,
but they can be simulated with strategic-match and zipper movement
strategies. {x1,...,xn : s} is (scope [x1 ... xn] s) in stratege.
