(ns stratege.cps
  "utilities for writing cps based strategies")

;;no special thunk implementation needed, it resulted in no performance benefits
;;compared to normal nullary functions and clojure.core trampoline
;;because cps is only used for the strategies that either return a state or nil
;;the test for fn? in trampoline is fine.



;; helper function for thunk based cps style

(defn call
  "returns a thunk that when called evaulates the continuation c with arg"
  ([c arg]
   #(c arg))
  ([c arg & args]
   #(apply c arg args)))


;; cps based strategies always take the continuation argument in the last slot.
;; let-cps transforms let-based bindings to cps style

(defmacro let-cps
  "example (let-cps [nstate (s state)] body) => (s state (fn [nstate] body))"
  [bindings & body-exprs]
  (assert (vector? bindings))
  (assert (even? (count bindings)))
  (if (empty? bindings)
    `(do ~@body-exprs)
    `(~'stratege.core/combine ~@(second bindings)
                               (fn [~(first bindings)]
                                 (let-cps ~(subvec bindings 2)
                                   ~@body-exprs)))))




(defn reduce-cps [f init coll k]
  (if (seq coll)
    (call f init (first coll) (fn [ns] (reduce-cps f ns (rest coll) k)))
    (call k init)))

