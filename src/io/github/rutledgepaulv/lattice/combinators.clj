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