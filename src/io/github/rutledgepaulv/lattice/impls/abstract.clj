(ns io.github.rutledgepaulv.lattice.impls.abstract
  "Define all the graph operations in terms of other abstract protocols.
   These serve as the only implementations unless there's a concrete
   implementation defined elsewhere for performance reasons. Concrete
   implementations included with this library can be found in the
   io.github.rutledgepaulv.lattice.impls.concrete namespace."
  (:require [clojure.set :as sets]
            [io.github.rutledgepaulv.lattice.protocols :as protos]))

(extend-protocol protos/ComputedNodes
  Object
  (nodes [this] #{}))

(extend-protocol protos/ComputedSuccessors
  Object
  (successors [this node]
    (into #{} (filter (fn [other] (contains? (protos/predecessors this other) node))) (protos/nodes this))))

(extend-protocol protos/ComputedPredecessors
  Object
  (predecessors [this node]
    (into #{} (filter (fn [other] (contains? (protos/successors this other) node))) (protos/nodes this))))

(extend-protocol protos/ComputedInboundEdges
  Object
  (inbound-edges [this node]
    (set (map vector (protos/predecessors this node) (repeat node)))))

(extend-protocol protos/ComputedOutboundEdges
  Object
  (outbound-edges [this node]
    (set (map vector (repeat node) (protos/successors this node)))))

(extend-protocol protos/ComputedAdjacency
  Object
  (adjacency [this]
    (reduce
      (fn [adjacency node]
        (assoc adjacency node (protos/successors this node)))
      {}
      (protos/nodes this))))

(extend-protocol protos/ComputedEdges
  Object
  (edges [this]
    (reduce
      (fn [edges node]
        (sets/union edges (protos/outbound-edges this node)))
      #{}
      (protos/nodes this))))

(extend-protocol protos/ComputedEdgesOf
  Object
  (edges-of [this node]
    (sets/union
      (protos/inbound-edges this node)
      (protos/outbound-edges this node))))

(extend-protocol protos/ComputedDegree
  Object
  (degree [this node]
    (count (protos/edges-of this node))))

(extend-protocol protos/ComputedNeighbors
  Object
  (neighbors [this node]
    (sets/union
      (protos/successors this node)
      (protos/predecessors this node))))

(extend-protocol protos/ComputedSources
  Object
  (sources [this]
    (into #{} (filter (fn [node] (empty? (protos/predecessors this node)))) (protos/nodes this))))

(extend-protocol protos/ComputedSinks
  Object
  (sinks [this]
    (into #{} (filter (fn [node] (empty? (protos/successors this node)))) (protos/nodes this))))

(extend-protocol protos/ComputedProducers
  Object
  (producers [this]
    (into #{} (remove (fn [node] (protos/successors this node))) (protos/nodes this))))

(extend-protocol protos/ComputedConsumers
  Object
  (consumers [this]
    (into #{} (remove (fn [node] (protos/predecessors this node))) (protos/nodes this))))

(extend-protocol protos/ComputedInterior
  Object
  (interior [this]
    (into #{}
          (remove
            (fn [node]
              (or (empty? (protos/successors this node))
                  (empty? (protos/predecessors this node)))))
          (protos/nodes this))))

(extend-protocol protos/ComputedInverse
  Object
  (inverse [this]
    (reify
      protos/ComputedNodes
      (nodes [_]
        (protos/nodes this))
      protos/ComputedSuccessors
      (successors [_ node]
        (protos/predecessors this node))
      protos/ComputedPredecessors
      (predecessors [_ node]
        (protos/successors this node)))))

(extend-protocol protos/ComputedDescendants
  Object
  (descendants [this node]
    (rest
      (tree-seq
        (comp not-empty (partial protos/successors this))
        (partial protos/successors this)
        node))))

(extend-protocol protos/ComputedAncestors
  Object
  (ancestors [this node]
    (rest
      (tree-seq
        (comp not-empty (partial protos/predecessors this))
        (partial protos/predecessors this)
        node))))

(extend-protocol protos/ComputedRoot
  Object
  (root [this]
    (first (protos/sources this))))

(extend-protocol protos/ComputedBranches
  Object
  (branches [this]
    (protos/producers this)))

(extend-protocol protos/ComputedLeaves
  Object
  (leaves [this]
    (protos/sinks this)))

(extend-protocol protos/ComputedParent
  Object
  (parent [this node]
    (first (protos/predecessors this node))))

(extend-protocol protos/ComputedChildren
  Object
  (children [this node]
    (protos/successors this node)))

(extend-protocol protos/ComputedOptimize
  Object
  (optimize [this]
    (let [memoized-nodes        (memoize (fn [] (protos/nodes this)))
          memoized-successors   (memoize (fn [node] (protos/successors this node)))
          memoized-predecessors (memoize (fn [node] (protos/predecessors this node)))]
      (reify
        protos/ComputedNodes
        (nodes [_]
          (memoized-nodes))
        protos/ComputedSuccessors
        (successors [_ node]
          (memoized-successors node))
        protos/ComputedPredecessors
        (predecessors [_ node]
          (memoized-predecessors node))))))

(extend-protocol protos/ComputedSupergraph
  Object
  (supergraph? [this other]
    (and
      (sets/superset? (protos/nodes this) (protos/nodes other))
      (sets/superset? (protos/edges this) (protos/edges other)))))

(extend-protocol protos/ComputedSubgraph
  Object
  (subgraph? [this other]
    (and
      (sets/subset? (protos/nodes this) (protos/nodes other))
      (sets/subset? (protos/edges this) (protos/edges other)))))

(extend-protocol protos/ComputeWithNode
  Object
  (with-node [this node-to-add]
    (reify
      protos/ComputedNodes
      (nodes [_]
        (conj (protos/nodes this) node-to-add))
      protos/ComputedSuccessors
      (successors [_ node]
        (protos/successors this node))
      protos/ComputedPredecessors
      (predecessors [_ node]
        (protos/predecessors this node)))))

(extend-protocol protos/ComputeWithEdge
  Object
  (with-edge [this source sink]
    (reify
      protos/ComputedNodes
      (nodes [_]
        (conj (protos/nodes this) source sink))
      protos/ComputedSuccessors
      (successors [_ node]
        (if (= node source)
          (conj (protos/successors this node) sink)
          (protos/successors this node)))
      protos/ComputedPredecessors
      (predecessors [_ node]
        (if (= node sink)
          (conj (protos/predecessors this node) source)
          (protos/predecessors this node))))))

(extend-protocol protos/ComputeWithoutNode
  Object
  (without-node [this node-to-remove]
    (reify
      protos/ComputedNodes
      (nodes [_]
        (disj (protos/nodes this) node-to-remove))
      protos/ComputedSuccessors
      (successors [_ node]
        (if (= node node-to-remove)
          #{}
          (disj (protos/successors this node) node-to-remove)))
      protos/ComputedPredecessors
      (predecessors [_ node]
        (if (= node node-to-remove)
          #{}
          (disj (protos/predecessors this node) node-to-remove))))))

(extend-protocol protos/ComputeWithoutEdge
  Object
  (without-edge [this source sink]
    (reify
      protos/ComputedNodes
      (nodes [_]
        (protos/nodes this))
      protos/ComputedSuccessors
      (successors [_ node]
        (if (= node source)
          (disj (protos/successors this node) sink)
          (protos/successors this node)))
      protos/ComputedPredecessors
      (predecessors [_ node]
        (if (= node sink)
          (disj (protos/predecessors this node) source)
          (protos/predecessors this node))))))


(extend-protocol protos/ComputeTopologicalSort
  Object
  (topological-sort [this]
    (loop [result  []
           graph   this
           pending (protos/sources this)]
      (if (empty? pending)
        (when (empty? (protos/edges graph))
          result)
        (let [s   (first pending)
              agg (reduce (fn [agg m]
                            (let [g' (protos/without-edge (get-in agg [:graph]) s m)]
                              (cond-> agg
                                :always (assoc :graph g')
                                (empty? (protos/inbound-edges g' m)) (update :pending conj m))))
                          {:graph graph :pending (disj pending s)}
                          (protos/successors graph s))]
          (recur (conj result s) (:graph agg) (:pending agg)))))))