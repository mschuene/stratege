(ns stratege.core
  "strategy combinators for term rewriting over zippers"
  (:refer-clojure :exclude [some replace repeat attempt])
  (:require [clj-tuple :as t]
            [clojure.core.match :as m]
            [clojure.set :as set]
            [fast-zip.core :as zip]
            ;[clojure.zip :as zip]
            [clojure.tools.macro :as ctm]
            [stratege.cps :refer [call let-cps reduce-cps]]))

;; a strategy takes a tuple of a binding map and a location together
;; with a continuation and calls it either with a new binding-map and
;; loc tuple (state) or with nil in case of failure. See the cps
;; namespace for utility functions to handle writing cps style thunk
;; returning functions that can be executed without consuming stack
;; via trampoline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply Strategy to term

;; stratege works by design on zippers. Because the choice of the
;; zipper can be different depending on the application, the zipper
;; functions are stored in the default binding-map. der default, the
;; fast zipper vector-zip is used.
(defrecord ZipperImpl [up down node right make-node root replace make-zip])

(def default-bindings
  "standard bindings for strategy invokations. Uses fast-zip for the zipper functions"
  (map->ZipperImpl {:zip-up zip/up
                    :zip-down zip/down
                    :zip-node zip/node
                    :zip-next zip/next
                    :zip-right zip/right
                    :zip-make-node zip/make-node
                    :zip-end? zip/end?
                    :zip-root zip/root
                    :zip-branch? zip/branch?
                    :zip-replace zip/replace
                    :zip-make-zip zip/vector-zip}))

(defn apply-strategy-cps
  "applies strategy to term with optional default bindings, discards
  resulting bindings and unpacks the zipper is normally called when
  invoking strategies as functions."
  ([strategy term] (apply-strategy-cps strategy term {}))
  ([strategy term bindings]
   (let [bindings (merge default-bindings bindings)]
     (when-let [res (trampoline
                     strategy (t/vector bindings ((:zip-make-zip bindings) term)) identity)]
       (-> res second zip/root)))))

;; it is convenient if one can see a strategy as simply a function
;; from term to term or nil for the external api while still having
;; the composability and stackless execution with the cps approach.
;; Therefore, Strategies are represented by a Strategy type that
;; implements IFn and has a combine method for the two calling methods

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
  "identity strategy, returns the term unchanged and does not create
  or modify bindings"
  (strategy [state k]
    (call k state)))

(def fail
  "fail strategy, always returns nil"
  (strategy [state k]
    (call k nil)))

;;;;;;; strategy combinators ;;;;;;;;

(defn <*
  "sequential composition of strategies.
   takes strategies as arguments and returns a strategy, that applies
  the first strategy, and if it succeeds the next and so on. If one
  strategy fails, it fails"
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
  "deterministic choice between strategies tries the strategies in
  order. Succeeds if one on them succeeds and fails otherwise"
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

(defn rec
  "to succinctly write recursive strategies using strategy
  combinators"
  [s & args]
  (strategy [state c] 
     (combine (apply s args) state c)))

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

(defn some
  "applies strategy s to the as much subterms as possible, but at
  least one."
  [s]
  (strategy [[b t :as state] k]
    (if-let [leftmost-child ((:zip-down b) t)]
      (letfn [(some-step [[b l :as state] succeeded-once?]
                (let-cps [[nb nl] (s state)]
                  (if nb
                    (if-let [right ((:zip-right b) nl)]
                      (some-step (t/vector nb right) true)
                      (call k (t/vector nb ((:zip-up b) nl))))
                    (if-let [right ((:zip-right b) l)]
                      (some-step (t/vector b right) succeeded-once?)
                      (call k (and succeeded-once? (t/vector b ((:zip-up b) l))))))))]
        (some-step (t/vector b leftmost-child) false))
      (call k nil))))

;; traversal strategies

(defn attempt
  "tries strategy, if it fails attempt becomes the identity strategy"
  [s]
  (<+ s id))

(defn repeat
  "repeatedly applies s until it fails"
  [s]
  (attempt (<* s (rec repeat s))))

(defn topdown
  "applies s in a top down fashion on the given term"
  [s]
  (<* s (all (rec topdown s))))

(defn bottomup
  "applies s in a bottom up fashon on the given term"
  [s]
  (<* (all (rec bottomup s)) s))

(defn leaves
  "like bottomup but only applies s when location is a leaf"
  [s]
  (bottomup (strategy [[b loc :as state] c]
              (if ((:zip-branch? b) loc)
                (call c state)
                (combine s state c)))))

(defn downup
  "applies s then descends and applies s again"
  [s]
  (<* s (all (rec downup s)) s))

(defn onebu
  "applies s bottom up on one child"
  [s]
  (<+ (one (rec onebu s)) s))

(defn somebu
  "applies s bottom up on some children"
  [s]
  (<+ (some (rec somebu s)) s))

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

(defn sometd
  "apply s to top level and if it fails on the level deeper"
  [s]
  (<+ s (some (rec sometd s))))

(defn innermost
  "applies s repeatedly bottomup until it can't be applied any more"
  [s]
  (bottomup (attempt (<* s (rec innermost s)))))

(defn on-node
  "applies f with [bindings (zip/node loc)] and replaces loc and bindings if f
  succeeded."
  [f]
  (strategy [[b loc] c]
    (c (when-let [[nb nt] (f (t/vector b ((:zip-node b) loc)))]
         (t/vector nb ((:zip-replace b) loc nt))))))

(defn on-loc
  "applies f with [bindings loc] and sets returned bindings and locatiion"
  [f]
  (strategy [state c]
    (c (when-let [nstate (f state)] nstate))))

(defn replace
  "a replace strategy operates on a node and neither uses nor create new bindings.
   f is a function from node to either nil or a new node, which is
  then replaced."
  [f]
  (on-node (fn [[b node]]
             (let [new-node (f node)]
               (if (not (nil? new-node))
                 (let [res (t/vector b new-node)]
                   res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Match Build and Variable Binding

(defn put-bindings
  "merges the current binding map with the new bindings"
  [new-bindings]
  (on-node (fn [[b t]] (t/vector (merge b new-bindings) t))))

(defn update-bindings
  "updates the current binding map with the given aguments"
  [& update-args]
  (on-node (fn [[b t]] (t/vector (apply update-in [b] update-args) t))))

(defn ?
  "if var-or-value is a symbol strating with ?, the current term is
  checked with the current binding associated to that symbol. If it is
  not equal, the application fails. If there is no binding associated
  to that symbol, it binds the current-term to the symbol"
  [var-or-value]
  (if (and (symbol? var-or-value)
           (.startsWith ^String (name var-or-value) "?")) 
    (on-node (fn [[b t :as state]]
               (if-let [value (get b var-or-value)]
                 (and (= value t) state)
                 (t/vector (assoc b var-or-value t) t))))
    (on-node (fn [[b t :as state]] (and (= t var-or-value) state)))))

(defn !
  "replaces the subject term with the instantiation of the pattern t
  using the current bindings of terms to variables in t. Only one
  bottomup walk is performed on t."
  ([to-build] (! to-build false))
  ([to-build as-loc?]
   (if as-loc?
     (strategy [[b loc] c]
       (call c (t/vector b ((bottomup (replace #(get b %))) to-build))))
     (on-node (fn [[b t]] (t/vector b ((bottomup (attempt (replace #(get b %)))) to-build)))))))

(defn scope
  "the scope operator limits the scope of the vars to the application
  of s."
  [vars s]
  (let [var-set (into #{} vars)]
    (strategy [[b t] k]
      (let [old-bindings (select-keys b vars)
            not-set (set/difference var-set (into #{} (keys b)))]
        (let-cps [nstate (s (t/vector (apply dissoc b vars) t))]
          (call k (when-let [[b2 nt] nstate]
                    (t/vector (reduce dissoc (merge b2 old-bindings) not-set) nt))))))))

(defn where
  "applies s to the subject term. if it succeeds, restores the
  original term but keeps the bindings."
  [s]
  (strategy [[b t :as state] k]
    (let-cps [[nb _] (s state)]
      (call k (when nb (t/vector nb t))))))

(defn guard
  "guards application of strategy.
  It is only applied if the current node fullfils predicate?.
  shorthand for (<* (where (replace predicate?)) strategy)"
  [predicate? strategy]
  (<* (where (replace #(or (predicate? %) nil))) strategy))

(defn emit-bindings
  "replaces current node with [current-bindings node]"
  ([] (on-node (fn [[b t]] (t/vector b (t/vector b t)))))
  ([vars] (on-node (fn [[b t]] (t/vector b (t/vector (select-keys b vars) t))))))

(defn debug
  "args is a vector indicating how f should be called. supported are
  keys :loc :node :bindings. Always succeeds. Example usage:
  (debug [:node] prn)"
  ([f] (replace #(do (f) %)))
  ([args f]
   (strategy [[b loc :as state] c]
     (apply f (map #(condp =  %
                      :loc loc
                      :node ((:zip-node b) loc)
                      :bindings b
                      nil) args))
     (call c state))))

;;match and replace

(defn build-match-on-form
  "builds the form the match is performed on for the strategic-match macro"
  [specification bs locs]
  (m/match specification
    :node (list (list :zip-node bs) locs)
    :loc locs
    :bindings bs
    [a & rest] (apply conj [(build-match-on-form a bs locs)]
                      (map #(build-match-on-form % bs locs) rest))
    :else (throw (Exception. "unsupported match-on form"))))

(defmacro strategic-match
  "matches on the current term, right hand sides are strategies.
   Matching is performed with core.match.
   First argument can be an option-map. Currently supported options are:
   :match-on - specifies on what the match call is made. Possible values are
   :node :loc :bindings :state or a vector containing this keys like [:node :bindings :loc].
   Defaults to :node."
  [& args]
  (let [options-map (merge {:match-on :node} (when (odd? (count args)) (first args)))
        args (if (odd? (count args)) (rest args) args)
        b# (gensym "b") loc# (gensym "loc")]
    `(strategy [[~b# ~loc# :as state#] c#]
       (let [res# (m/match ~(build-match-on-form (:match-on options-map) b# loc#)
                    ~@args :else fail)]
         (combine res# state# c#)))))

(defmacro match-replace
  "matches on the current term and replaces it by an instantiation of the right hand side.
   Matching is performed with core.match. Special case of
   strategic-match where all right hand sides are wrapped in (replace (constantly rhs)).
   Accepts an option map as optional first argument. See strategic-match for details."
  [& args]
  `(strategic-match
    ~@(map (fn [i arg] (if (odd? i) `(replace (constantly ~arg)) arg))
         (range) (if (map? (first args)) (rest args) args))))

(defrecord Rule [lhspat rhspat f options-map]
  IStrategy
  (combine [this state continuation]
    (.combine f state continuation))
  clojure.lang.IFn
  (invoke [this term] (.invoke f term))
  (invoke [this term zip] (.invoke f term zip))
  (applyTo [this arglist] (.applyTo f this arglist)))

(defmacro strategic-rule
  "a rule where the right hand side is a strategy, which will be
  applied next if the lhs matched. Accepts an option map as optional
  first argument. See strategic-match for details."
  ([lhs -> rhs] `(strategic-rule {:match-on :node} ~lhs -> ~rhs))
  ([options-map lhs -> rhs]
   `(->Rule ~(list 'quote lhs) ~(list 'quote rhs)
            (strategic-match ~options-map ~lhs  ~rhs)
            options-map)))

(defmacro defstrategic-rule [a & rest]
  (let [[name args] (ctm/name-with-attributes a rest)]
    `(def ~name (strategic-rule ~@args))))

(defmacro rule
  "a rule that replace
  match. Accepts an option map as optional first argument. See
  strategic-match for details."
  ([lhs -> rhs] `(rule {:match-on :node} ~lhs -> ~rhs))
  ([options-map lhs -> rhs]
   `(->Rule ~(list 'quote lhs) ~(list 'quote `(replace (constantly ~rhs)))
            (match-replace ~options-map ~lhs  ~rhs)
            ~options-map)))

(defmacro defrule [name & rest]
  (let [[name args] (ctm/name-with-attributes name rest)]
    `(def ~name (rule ~@args))))

(defn build-rule
  "builds rule from rule-form"
  [rf]
  (cond (and (sequential? rf)
             (= #'stratege.core/strategic-rule (resolve (first rf))))
        (condp = (count rf)
          4 (->Rule (nth rf 1) (nth rf 3) nil {:match-on :node})
          5 (->Rule (nth rf 2) (nth rf 4)
                    {:loc-as (nth rf 1) :bindings-as (nth rf 1)}))
        (and (sequential? rf)
             (= #'stratege.core/rule (resolve (first rf))))
        (condp = (count rf)
          4 (->Rule (nth rf 1) `(replace (constantly ~(nth rf 3))) nil {:match-on :node})
          5 (->Rule (nth rf 1) `(replace (constantly ~(nth rf 4)))
                    {:loc-as (nth rf 1) :bindings-as (nth rf 1)}))
        :else rf))

;;to make rules work with the :match-on option, one has to
;; 1. determine the superset of keys to match on from all the rules
;;   - core.match is optimized enough to just match on all possible
;;     combinations [:node :loc :bindings]
;; 2. modify every lhs to be consistent with what is matched-on

(defn build-rules-lhs
  "builds the lhs of the match for the rule for the rules macro"
  [rule]
  (let [matched-on (:match-on (:options-map rule))
        match-on-map (if (keyword? matched-on) {matched-on (:lhspat rule)}
                         (zipmap matched-on (:lhspat rule)))
        or-gensym (fn [f] (fn [v] (let [res (f v)] (if (not (nil? res)) res (gensym (str f))))))]
    ((juxt (or-gensym :node) (or-gensym :loc) (or-gensym :bindings)) match-on-map)))

(defmacro rules
  "every arg should be either a (rule ...) or (strategic-rule ...)
  form or a globally bound variable to a rule. WARNING: Does not
  backtrack when application of one rhs fails, use (<+ (strategic-rule ..)
  (rule ....) ...) instead."
  [& rules]
  (let [rules (->> rules
                   (map build-rule)
                   (map #(if (symbol? %)
                          (if-let [var (resolve %)]
                            (var-get var)
                            (assert false (str "couldn't resolve " % " to a rule")))
                          %)))]
    `(strategy [[b# loc# :as state#] c#]
       (combine (m/match [((:zip-node b#) loc#) loc# b#]
                  ~@(interleave
                     (map build-rules-lhs rules)
                     (map :rhspat rules))
                  :else fail) state# c#))))

(defmacro defrules [name & rest]
  (let [[name args] (ctm/name-with-attributes name rest)]
    `(def ~name (rules ~@args))))


;; Local Variables:
;; eval: (put-clojure-indent 'let-cps 1)
;; eval: (put-clojure-indent 'strategy 1)
;; eval: (put-clojure-indent 'match 1)
;; End:
