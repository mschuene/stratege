(ns stratege.term-zip
  (:require [clojure.zip :refer [zipper]]))

(defprotocol PTerm
  (head [this])
  (accepts-head [this head])
  (branch? [this])
  (children [this])
  (create [this children]))

(extend-protocol PTerm
  Object
  (head [this] (type this))
  (accepts-head [this head]
    (or (= (head this) head)
        ((supers (class this)) head)))
  (children [this]
    (try (or (mapv #(clojure.lang.Reflector/getInstanceField this (str %))
                   (clojure.lang.Reflector/invokeStaticMethod
                    (class this) "getBasis" (into-array Object [])))
             (seq this))
         (catch Exception e [])))
  (branch? [this] (seq (children this))) ;;make more performant
  (create [this children]
    (try (into (empty this) children)
         (catch Exception e
           (clojure.lang.Reflector/invokeConstructor
            (class this) (into-array Object children)))))
  clojure.lang.IPersistentVector
  (head [this] :vector)
  (accepts-head [this head]
    (or (= head :vector)
        (= head :seq)
        (= (type this) head)
        ((supers (class this)) head))))


(defn term-zip [root]
  (zipper branch?
          children
          create
          root))
