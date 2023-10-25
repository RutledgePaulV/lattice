(ns io.github.rutledgepaulv.lattice.combinators
  "Functions for combining graphs in various abstract forms. No attempt
   is made to provide concrete implementations of these functions, but you
   may be interested in running io.github.rutledgepaulv.lattice.core/optimize
   on the result of these functions to get an optimized version."
  (:require [clojure.set :as sets]
            [io.github.rutledgepaulv.lattice.protocols :as protos]))

(defn- combine [init combinator graphs]
  (reify
    protos/ComputedNodes
    (nodes [_]
      (reduce combinator init (map protos/nodes graphs)))
    protos/ComputedSuccessors
    (successors [_ node]
      (reduce combinator init (map (fn [graph] (protos/successors graph node)) graphs)))
    protos/ComputedPredecessors
    (predecessors [_ node]
      (reduce combinator init (map (fn [graph] (protos/predecessors graph node)) graphs)))))

(defn union [graphs]
  (combine #{} sets/union graphs))

(defn intersection [graphs]
  (combine #{} sets/intersection graphs))

(defn difference [graphs]
  (combine #{} sets/difference graphs))

(defn deep-merge
  ([])
  ([a] a)
  ([a b]
   (when (or a b)
     (letfn [(merge-entry [m e]
               (let [k  (key e)
                     v' (val e)]
                 (if (contains? m k)
                   (assoc m k (let [v (get m k)]
                                (if (and (map? v) (map? v'))
                                  (deep-merge v v')
                                  v')))
                   (assoc m k v'))))]
       (reduce merge-entry (or a {}) (seq b)))))
  ([a b & more]
   (reduce deep-merge (or a {}) (cons b more))))