(ns io.github.rutledgepaulv.lattice.utils
  "Functions for combining graphs in various abstract forms. No attempt
   is made to provide concrete implementations of these functions, but you
   may be interested in running io.github.rutledgepaulv.lattice.core/optimize
   on the result of these functions to get an optimized version."
  (:require [clojure.set :as sets]
            [io.github.rutledgepaulv.lattice.protocols :as protos])
  (:refer-clojure :exclude [empty])
  (:import (clojure.lang PersistentQueue)
           (io.github.rutledgepaulv.lattice.protocols Graph)))

(defn empty []
  (reify
    Graph
    protos/ComputeNodes
    (nodes [_] #{})
    protos/ComputeSuccessors
    (successors [_ node] #{})
    protos/ComputePredecessors
    (predecessors [_ node] #{})))

(defn- combine [combinator graphs]
  (let [nodes (delay (reduce combinator (map protos/nodes graphs)))]
    (reify
      Graph
      protos/ComputeNodes
      (nodes [_]
        (force nodes))
      protos/ComputeSuccessors
      (successors [_ node]
        (if (contains? (force nodes) node)
          (reduce combinator (map (fn [graph] (protos/successors graph node)) graphs))
          #{}))
      protos/ComputePredecessors
      (predecessors [_ node]
        (if (contains? (force nodes) node)
          (reduce combinator (map (fn [graph] (protos/predecessors graph node)) graphs))
          #{})))))

(defn union [graphs]
  (combine sets/union graphs))

(defn intersection [graphs]
  (combine sets/intersection graphs))

(defn difference [graphs]
  (combine sets/difference graphs))

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

(defn graph-seq-depth-first [branch? children root]
  (let [sentinel (Object.)]
    (->> (iterate
           (fn [{:keys [queue visited] :as agg}]
             (if (empty? queue)
               (assoc agg :node sentinel)
               (let [head (peek queue)]
                 (if (contains? visited head)
                   (assoc agg :queue (pop queue))
                   (cond-> agg
                     (branch? head)
                     (assoc :queue (-> PersistentQueue/EMPTY (into (children head)) (into (pop queue))))
                     :always (-> (assoc :visited (conj visited head)) (assoc :node head)))))))
           {:queue   (into PersistentQueue/EMPTY (list root))
            :visited #{}})
         (rest)
         (map :node)
         (take-while (clojure.core/complement #{sentinel}))
         (dedupe))))

(defn graph-seq-breadth-first [branch? children root]
  (let [sentinel (Object.)]
    (->> (iterate
           (fn [{:keys [queue visited] :as agg}]
             (if (empty? queue)
               (assoc agg :node sentinel)
               (let [head (peek queue)]
                 (if (contains? visited head)
                   (assoc agg :queue (pop queue))
                   (cond-> agg
                     (branch? head)
                     (assoc :queue (into (pop queue) (children head)))
                     :always (-> (assoc :visited (conj visited head)) (assoc :node head)))))))
           {:queue   (into PersistentQueue/EMPTY (list root))
            :visited #{}})
         (rest)
         (map :node)
         (take-while (clojure.core/complement #{sentinel}))
         (dedupe))))
