(ns stratege.benchmark
  (:require  [clojure.test :refer :all]
             [fast-zip.core :as zip]
             [stratege.cps :refer :all]
             [criterium.core :as crit]
             [stratege.core :as s]))



;;compare strategy combinator defined bottomup to hand rolled zip based bottomup

(defn apply-strategy-bottomup [s term]
  (loop [[loc b :as state] [(zip/vector-zip term) s/default-bindings]
         pass :down]
    (condp = pass
      :down
      (if ((:zip-branch? b) loc)
        (recur [((:zip-down b) loc) b] :down)
        ;;go right
        (recur state :right))
      :right ;;applies rule and goes right
      (when-let [[nloc nb] (s state)]
        (if-let [right ((:zip-right nb) nloc)]
          (recur [right nb] :down)
          (if-let [up ((:zip-up nb) nloc)]
            (recur [up nb] :right)
            ;;we are done
            (zip/root loc)))))))


(defn fast-bottomup [s]
  (s/strategy [state c]
    (letfn [(fb-step [[b loc :as state] pass]
              (condp = pass
                :down
                (if ((:zip-branch? b) loc)
                  (recur [b ((:zip-down b) loc)] :down)
                  ;;go right
                  (recur state :right))
                :right ;;applies rule and goes right
                (let-cps [[nb nloc] (s state)]
                  (if nloc
                    (if-let [right ((:zip-right nb) nloc)]
                      (fb-step [nb right] :down)
                      (if-let [up ((:zip-up nb) nloc)]
                        (fb-step [nb up] :right)
                        (call c [nb nloc])))
                    (call c nil)))))]
      (fb-step state :down))))

(time (def res-by-hand
        (apply-strategy-bottomup (fn [[loc b]] [(zip/replace loc [(zip/node loc) "hi"]) b]) (nth (iterate vector 1) 100000))))

(time (def res-by-combinators-1
        ((fast-bottomup
          (s/strategy [[b loc :as state] c]
            #(c [b (zip/replace loc [(zip/node loc) "hi"]) b])))
         (nth (iterate vector 1) 100000))))

(time (def res-by-combinators-2
        ((s/bottomup
          (s/strategy [[b loc :as state] c]
            #(c [b (zip/replace loc [(zip/node loc) "hi"]) b])))
         (nth (iterate vector 1) 100000))))

"Evaluation count : 720 in 60 samples of 12 calls.
             Execution time mean : 89.775128 ms
    Execution time std-deviation : 3.056586 ms
   Execution time lower quantile : 85.952873 ms ( 2.5%)
   Execution time upper quantile : 93.937951 ms (97.5%)
                   Overhead used : 1.720740 ns
Evaluation count : 660 in 60 samples of 11 calls.
             Execution time mean : 98.577456 ms
    Execution time std-deviation : 3.033710 ms
   Execution time lower quantile : 93.575802 ms ( 2.5%)
   Execution time upper quantile : 102.114751 ms (97.5%)
                   Overhead used : 1.720740 ns
Evaluation count : 420 in 60 samples of 7 calls.
             Execution time mean : 170.709931 ms
    Execution time std-deviation : 4.702519 ms
   Execution time lower quantile : 163.323508 ms ( 2.5%)
   Execution time upper quantile : 177.857952 ms (97.5%)
                   Overhead used : 1.720740 ns
nil"

;; with a more costly strategy
(time (do (def res-by-hand-longer (apply-strategy-bottomup (fn [state] (Thread/sleep 1) state) (nth (iterate vector 1) 1000))) "hi"))

(time (do (def res-by-combinators-1-longer
            ((fast-bottomup
              (s/strategy [state c]
                (Thread/sleep 1) #(c state)))
             (nth (iterate vector 1) 1000))) "hi"))

(time (do (def res-by-combinators-2-longer
            ((s/bottomup
              (s/strategy [state c]
                (Thread/sleep 1) #(c state)))
             (nth (iterate vector 1) 1000))) "hi"))

"Evaluation count : 60 in 60 samples of 1 calls.
             Execution time mean : 1.110024 sec
    Execution time std-deviation : 5.142785 ms
   Execution time lower quantile : 1.098260 sec ( 2.5%)
   Execution time upper quantile : 1.120426 sec (97.5%)
                   Overhead used : 1.720740 ns

Found 3 outliers in 60 samples (5.0000 %)
	low-severe	 2 (3.3333 %)
	low-mild	 1 (1.6667 %)
 Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
Evaluation count : 60 in 60 samples of 1 calls.
             Execution time mean : 1.111702 sec
    Execution time std-deviation : 5.579326 ms
   Execution time lower quantile : 1.096689 sec ( 2.5%)
   Execution time upper quantile : 1.120777 sec (97.5%)
                   Overhead used : 1.720740 ns

Found 2 outliers in 60 samples (3.3333 %)
	low-severe	 2 (3.3333 %)
 Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
Evaluation count : 60 in 60 samples of 1 calls.
             Execution time mean : 1.120280 sec
    Execution time std-deviation : 9.007534 ms
   Execution time lower quantile : 1.102646 sec ( 2.5%)
   Execution time upper quantile : 1.139102 sec (97.5%)
                   Overhead used : 1.720740 ns

Found 3 outliers in 60 samples (5.0000 %)
	low-severe	 1 (1.6667 %)
	low-mild	 2 (3.3333 %)
 Variance from outliers : 1.6389 % Variance is slightly inflated by outliers"
nil
