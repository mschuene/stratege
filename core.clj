(ns stratege.core
  "strategy combinators for term rewriting over zippers"
  (:require [clj-tuple :as t]
            [clojure.core.match
             :refer [match to-source emit-pattern-for-syntax emit-pattern]]
            [clojure.set :as set]
            [riddley.walk :refer [macroexpand-all]]
            [fast-zip.core :as zip]
            [criterium.core :as crit]
            [stratege.cps :refer [call let-cps reduce-cps]]))

;; a strategy takes a tuple of a binding map and a location together
;; with a continuation and calls it either with a new binding-map and loc tuple (state)
;; or with nil in case of failure. See the cps namespace for utility functions to handle
;; writing cps style thunk returning functions that can be executed without consuming stack via
;; trampoline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply Strategy to term


;; the next question is how to handle the zippers.
;; the zipper implementation should be customary.
;; allow specification of zipper functions in the binding map

(defrecord ZipperImpl [up down node right make-node root replace make-zip])

(def default-bindings
  "standard bindings for strategy invokations. Uses fast-zip for the zipper functions"
  (map->ZipperImpl {:zip-up zip/up
                    :zip-down zip/down
                    :zip-node zip/node
                    :zip-right zip/right
                    :zip-make-node zip/make-node
                    :zip-root zip/root
                    :zip-replace zip/replace
                    :zip-make-zip zip/vector-zip}))

(defn apply-strategy-cps
  "applies strategy to term with optional default bindings, discards
  resulting bindings and unpacks the zipper is normally called when
  invoking strategies as functions."
  ([strategy term] (apply-strategy-cps strategy term {}))
  ([strategy term bindings]
   (when-let [res (trampoline strategy
                              (t/vector (merge default-zipper bindings)
                                        (zip/vector-zip term)) identity)]
     (-> res second zip/root))))


;; it is convenient if one can see a strategy as simply a function
;; from term to term or nil for the external api while still having
;; the composability and stackless execution the approach from the
;; last paragraph gives. Therefore, Strategies are represented by a Strategy type
;; that implements IFn and has a combine method.

(defprotocol IStrategy
  (combine [this state continuation]))

(deftype Strategy [f]
  IStrategy
  (combine [this state continuation]
    (f state continuation))
  clojure.lang.IFn
  (invoke [this term]
    (apply-strategy-cps f term))
  (invoke [this term zip] (apply-strategy-cps f term zip))
  (applyTo [this arglist] (apply apply-strategy-cps f arglist)))

(defmacro strategy
  [arg-vector & body]
 `(Strategy. (fn ~arg-vector ~@body)))  


;;;;;;; base strategies ;;;;;;;;

(def id
  "identity strategy, returns the term unchanged and does not create or modify bindings"
  (strategy [state k]
    (call k state)))

(def fail
  "fail strategy, always returns nil"
  (strategy [state k]
    (call k nil)))


;;;;;;; strategy combinators ;;;;;;;;

(defn <*
  "sequential composition of strategies.
   takes strategies as arguments and returns a strategy, that applies the first
   strategy, and if it succeeds the next and so on. If one strategy fails, it fails"
  [& strategies]
  (strategy [state k]
    (reduce-cps
     (fn [state s c]
       (let-cps [nstate (s state)]
                (if nstate
                  (call c nstate)
                  (call k nil)))) ;;short circuiting :)
     state strategies k)))


(defn <+
  "deterministic choice between strategies
   tries the strategies in order. Succeeds if one on them succeeds and fails otherwise"
  [& strategies]
  (strategy [state k]
    (reduce-cps
     (fn [state s c]
       (let-cps [nstate (s state)]
                (if nstate
                  (call k nstate)
                  (call c state))))
     state strategies (constantly (call k nil)))))


(defn negation
  "acts like id s fails or fail if s succeeds."
  [s]
  (strategy [state k]
    (let-cps [nstate (s state)]
      (call k (if nstate nil state)))))

(defmacro rec
  "to succinctly write recursive strategies using strategy combinators"
  [& args]
  `(strategy [state# c#] 
     (combine (~@args) state# c#)))


;; movement functions

(defn zip-op [op-key]
  (strategy [[b loc :as state] k]
    (call k (when-let [nloc ((op-key b) loc)]
              (t/vector b nloc)))))

(def zip-up (zip-op :zip-up))

(def zip-down (zip-op :zip-down))

(def zip-right (zip-op :zip-right))

(def zip-left (zip-op :zip-left))


(defn all
  "applies strategy s to all childrens of the current loc.
   Succeeds if and only if all invokation succeed."
  [s]
  (strategy [[b t :as state] k]
            (if-let [leftmost-child ((:zip-down b) t)]
      (letfn [(all-step [state]
                (let-cps [[nb nl] (s state)]
                         (if nb
                           (if-let [r ((:zip-right b) nl)]
                             (all-step (t/vector nb r))
                             (call k (t/vector nb ((:zip-up b) nl))))
                           (call k nil))))]
        (all-step (t/vector b leftmost-child)))
      (call k state))))

(defn one
  "applies strategy s to one child of the current loc.
   Fails if s fails on all subterms or the loc is leaf."
  [s]
  (strategy [[b t :as state] k]
            (if-let [leftmost-child ((:zip-down b) t)]
      (letfn [(one-step [[b l :as state]]
                (let-cps [[nb nl] (s state)]
                  (if nb
                    (call k (t/vector nb ((:zip-up b) nl)))
                    (if-let [r ((:zip-right b) l)]
                      (one-step (t/vector b r))
                      (call k nil)))))]
        (one-step (t/vector b leftmost-child)))
      (call k nil))))

(defn most
  "applies strategy s to the as much subterms as possible, but at least one."
  [s]
  (strategy [[b t :as state] k]
    (if-let [leftmost-child ((:zip-down b) t)]
      (letfn [(most-step [[b l :as state] succeeded-once?]
                (let-cps [[nb nl] (s state)]
                  (if nb
                    (if-let [right ((:zip-right b) nl)]
                      (most-step (t/vector nb right) true)
                      (call k (t/vector nb ((:zip-up b) nl))))
                    (if-let [right ((:zip-right b) l)]
                      (most-step (t/vector b right) succeeded-once?)
                      (call k (and succeeded-once? (t/vector b ((:zip-up b) l))))))))]
        (most-step (t/vector b leftmost-child) false))
      (call k nil))))



;; traversal strategies

(defn attempt
  "tries strategy, if it fails try becomes the identity strategy"
  [s]
  (<+ s id))

(defn exhaustively
  "exhaustively applies s until it fails"
  [s]
  (attempt (<* s (rec exhaustively s))))

(defn topdown
  "applies s in a top down fashion on the given term"
  [s]
  (<* s (all (rec topdown s))))

(defn bottomup
  "applies s in a bottom up fashon on the given term"
  [s]
  (<* (all (rec bottomup s)) s))

(defn downup
  "applies s then descends and applies s again"
  [s]
  (<* s (all (rec downup s)) s))

(defn onebu
  "applies s bottom up on one child"
  [s]
  (<+ (one (rec onebu s)) s))

(defn mostbu
  "applies s bottom up on most children"
  [s]
  (<+ (most (rec mostbu s)) s))

(defn downup2
  "applies s1 on descend and s2 on ascend"
  [s1 s2]
  (<* s1 (all (rec downup2 s1 s2)) s2))

(defn alltd
  "apply s to top level and if it fails on the level deeper"
  [s]
  (<+ s (all (rec alltd s))))


(defn onetd
  "apply s to top level and if it fails on the level deeper"
  [s]
  (<+ s (one (rec onetd s))))


(defn mosttd
  "apply s to top level and if it fails on the level deeper"
  [s]
  (<+ s (most (rec mosttd s))))


(defn innermost
  "applies s repeatedly bottomup until normal form is reached"
  [s]
  (bottomup (attempt (<* s (rec innermost s)))))

(defn on-node
  "applies f with [bindings (zip/node loc)] and replaces loc after f succeeded."
  [f]
  (strategy [[b loc] c]
    (c (when-let [[nb nt] (f (t/vector b ((:zip-node b) loc)))]
         (t/vector nb ((:zip-replace b) loc nt))))))


(defn simple
  "a simple strategy operates on a node and neither uses nor create new bindings.
   f is a function from node to either nil or a new node, which is then replaced."
  [f]
  (on-node (fn [[b node]]
             (when-let [new-node (f node)]
               (t/vector b new-node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Match Build and Variable Binding
;; TODO what is an appropriate syntax for matching terms
;; ((? '(? 1 ? 3)) (+ 1 2 3))
;; explanation:
;; () match sequences, so they are applied to the children of the current zipper
;; (? '[Vector. 1 2 3]) [1 2 3]) matches,but not on (list 1 2 3)
;; by the way, how to support arbitrary matches?
;; don't have to build this in in ? because it can be user defined using other strategy combinators
;; and also strategy composition:
;; (guard (simple (arbitrary-clojure-predicate-matching top-level-term)) (? .........))
;; and also arbitrary predicates on lower levels
;; (<* ?[Vector. ?a 2 3] (where (<*(! ?a) (guard (simple (arbitrary-clojure-predicte))) ......)

;;;;;;;;;;;;; desugaring ? terms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (? '(x1 ... xn)) => (*> down (? x1) right .... right (? xn) (negation right) up)
;; (? '[head. x1 ... xn] => (*> (check-head head.) down (? x1) ................
;; (? '{:a '?a} => (*> (match-key :a ?a) .........)


;;;;;;;;;;;; using core.match ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; it would be great if core.match could be used here. It has
;; expressive pattern forms and is very very efficient. But there are
;; still some issues about having very many patterns:

;;1. code size -> at some point the method size becomes too big and
;; the code doesn't compile anymore this happens especially fast when
;; :seq and rest arguments are often used.

;;2. it uses normal symbols (like a) to stand for variables or values
;; depending on whether they are bound or not. But with the bindings
;; coming from the binding part of the state, the bound variables
;; aren't known at compile time. there are solutions, either compare
;; the assigned variables afterwards with the already bound ones or
;; substitute the bound ones with their values in the pattern term
;; beforehand, which may give better performance.

;; 3. It would also be nice when vector matching would work on seqs
;; too, because this would be the normal case if stratege is used for
;; a cas like system like it is planned with expresso.

;; so for now, ? is only implemented in the trivial case of matching
;; either a constant or a variable and a rules strategy is provided
;; that just uses core.match

(defn ?
  "matches against t. works as identity strategy but creates new
  bindings or fails if match fails. If t is a symbol starting with a
  ?, it is considered a variable and is bound to the value except it
  is already bound in the current binding map to a different value. If
  not it is assumed as constant and compared with the current term or
  loc and succeeds iff the two are equal"
  ([var-or-value] (? var-or-value false))
  ([var-or-value match-on-loc?]
   (match [(and (symbol? var-or-value)
                (.startsWith ^String (name var-or-value) "?")) match-on-loc?]
     [true true] (strategy [[b loc :as state] c]
                   (if-let [value (get b var-or-value)]
                     (call c (and (= value loc) state))
                     (call c (t/vector (assoc b var-or-value loc) loc))))
     [true false] (on-node (fn [[b t :as state]]
                             (if-let [value (get b var-or-value)]
                               (and (= value t) state)
                               (t/vector (assoc b var-or-value t) t))))
     [false true] (strategy [[b loc :as state] c]
                    (call c (and (= loc var-or-value) state)))
     [false false] (on-node (fn [[b t :as state]] (and (= t var-or-value) state))))))



(defn !
  "replaces the subject term with the instantiation of the pattern
  t using the current bindings of terms to variables in t. Only one bottomup walk is performed on t."
  ([to-build] (! to-build false))
   ([to-build as-loc?]
   (if as-loc?
     (strategy [[b loc] c]
       (call c (t/vector b ((bottomup (simple #(get b %))) to-build))))
     (on-node (fn [[b t]] (t/vector b ((bottomup (attempt (simple #(get b %)))) to-build)))))))


(defn scope
  "the scope operator limits the scope of the vars to the application of s."
  [vars s]
  (let [var-set (into #{} vars)]
    (strategy [[b t] k]
      (let [old-bindings (select-keys b vars)
            not-set (set/difference var-set (into #{} (keys b)))]
        (let-cps [nstate (s (t/vector (apply dissoc b vars) t))]
          (call k (when-let [[b2 nt] nstate]
                    (t/vector (reduce dissoc (merge b2 old-bindings) not-set) nt))))))))


(defn where
  "applies s to the subject term. if it succeeds, restores the original
  term but keeps the bindings."
  [s]
  (strategy [[b t :as state] k]
    (let-cps [[nb _] (s state)]
      (call k (when nb (t/vector nb t))))))

(defn guard
  "guards application of strategy. It is only applied if the current node fullfils predicate?.
   shorthand for (<* (where (simple predicate?)) strategy)"
  [predicate? strategy]
  (<* (where (simple predicate?)) strategy))


(def emit-bindings
  "replaces current node with [current-bindings node]"
  (on-node (fn [[b t]] [b [b t]])))


(defprotocol PCongruence
  (accept-head [this head]
    "returns true if the type can be identified ad the provided head/type")
  (get-fields [this]
    "returns a vector of the fields of this type in order."))

(extend-protocol PCongruence
  Object
  (accept-head [this head]
    (and (class? head) (instance? head this)))
  (get-fields [this]
    (mapv #(. this %) (. (class this) getBasis)))
  clojure.lang.ISeq
  (accept-head [this head]
    (or (and (class? head) (instance? head this)))))


(defprotocol PTerm
  (head [this])
  (accepts-head [this head])
  (children [this]))

(extend-protocol PTerm
  Object
  (head [this] (= head (type this)))
  (accepts-head [this head]
    (or (= (type this) head)
        ((supers (class this)) head)))
  (children [this] [])
  clojure.lang.IPersistentVector
  (head [this] :vector)
  (accepts-head [this head]
    (or (= head :vector)
        (= head :seq)
        (= (type this) head)
        ((supers (class this)) head))))

;;todo make it work even when zipper isn't friendly
(defn con
  "congruence relation. If the current term matches the head (or is an instance
   of head)."
  [head & strategies]
  (strategy [[b loc :as state] c]
    (if (accepts-head ((:zip-node b) loc) head)
      (if-let [leftmost-child ((:zip-down b) loc)]
        (letfn [(con-step [[b loc :as state] strategies end?]
                  (if (seq strategies)
                    (let [s (first strategies)]
                      (if (not end?)
                        (let-cps [[b loc :as nstate] (s state)]
                                 (if (nil? nstate)
                                   (call c nil)
                                   (if-let [right ((:zip-right b) loc)]
                                     (con-step
                                      (t/vector b right) (rest strategies) false)
                                     (con-step
                                      (t/vector b loc) (rest strategies) true))))
                        (call c nil)))
                    (call c (t/vector b ((:zip-up b) loc)))))]
          (con-step (t/vector b leftmost-child) strategies false))
        (call c nil)
        ;;version without zippers, loses zipper context during child exprs but better than failing todo.
#_        (letfn [(con-step [[b loc :as state] strategies children]
                  (if (seq strategies)
                    (let [s (first strategies) c (first children)]
                      (if (not end?)
                        (let-cps [[b loc :as nstate] (s (t/vector b ((:make-zip b) c)))t]
                                 (if (nil? nstate)
                                   (call c nil)
                                   (con-step nstate (rest strategies) (rest children))))
                        (call c nil)))
                    (call c (t/vector b ((:zip-up b) loc)))))]
          (con-step (t/vector b loc) strategies false (children ((:zip-node b) loc)))))
      (call c nil))))

(defmacro match-replace [& args]
  (let [options-map (or (and (map? (first args)) (first args)) {})
        args (if (map? (first args)) (rest args) args)
        b# (gensym "b") loc# (gensym "loc")]
    `(strategy [[~b# ~loc#] c#]
       (let [~@(if-let [bs (:bindings-as options-map)] [bs b#])
             ~@(if-let [locs (:loc-as options-map)] [locs loc#])]
         (call c# (t/vector ~b# ((:zip-replace ~b#)
                                ~loc# (match [((zip ~b# :zip-node) ~loc#)]
                                        ~@args))))))))

(defrecord Rule [lhspat rhspat f]
  IStrategy
  (combine [this state continuation]
    (.combine f state continuation))
  clojure.lang.IFn
  (invoke [this term] (.invoke f term))
  (invoke [this term zip] (.invoke f term zip))
  (applyTo [this arglist] (.applyTo f this arglist)))


(defmacro rule [lhs -> rhs]
  `(->Rule ~(list 'quote lhs) ~(list 'quote rhs)
           (simple (fn [x#] (match [x#] [~lhs] ~rhs
                                   :else nil)))))
;; TODO use tools.macro
(defmacro defrule [a & rest]
  `(def ~a (rule ~@rest)))


(defmacro ruleset
  "every arg should be either a (rule ) form or a globally bound variable
   to a rule. semantically equivalent to (apply <+ rules) but much more efficient."
  [& rules]
  (let [rules (->> rules
                   (map #(if (and (sequential? %) (= #'stratege.core/rule (resolve (first %))))
                           (->Rule (nth % 1) (nth % 2) nil) %))
                   (map #(or (when-let [var (and (symbol? %) (resolve %))]
                               (var-get var)) %)))]
    `(simple (fn [x#]
               (match [x#]
                 ~@(interleave (map (comp vector :lhspat) rules) (map :rhspat rules))
                 :else nil)))))

(defrecord LocPattern [node path])

(defmethod emit-pattern-for-syntax [:loc :default]
  [[_ a b]]
  (emit-pattern-for-syntax (list [a b] :<< '(juxt zip/node identity)))) 

;; TODO make ? use core.match -> as action put all ?-starting variables in the binding list


;; Local Variables:
;; eval: (put-clojure-indent 'let-cps 1)
;; eval: (put-clojure-indent 'strategy 1)
;; eval: (put-clojure-indent 'match 1)
;; End
