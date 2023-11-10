(ns io.github.rutledgepaulv.lattice.protocols
  "Protocols for defining a graph. They are intentionally
   very granular so that implementations can be as efficient
   as possible. Default implementations are defined for Object
   (assumed to be something that implements at least `nodes` and
   one of `successors` or `predecessors`). Default implementations
   of `successors` is also defined for IPersistentMap."
  (:refer-clojure :exclude [ancestors descendants complement]))


; a java interface for marking reifications
; so that we can implement print-dup and
; print-method on reifications
(definterface Graph)

(defprotocol Edge
  "A protocol for defining an edge."
  (source [this]
    "Returns the source node of the edge.")
  (sink [this]
    "Returns the sink node of the edge."))

(defprotocol ComputeNodes
  (nodes [this]
    "Returns the nodes of the graph as a set."))

(defprotocol ComputeSuccessors
  (successors [this node]
    "Returns the nodes that follow the given node."))

(defprotocol ComputePredecessors
  (predecessors [this node]
    "Returns the nodes that precede the given node."))

(defprotocol ComputeInboundEdges
  (inbound-edges [this node]
    "Returns the inbound edges of the given node."))

(defprotocol ComputeOutboundEdges
  (outbound-edges [this node]
    "Returns the outbound edges of the given node."))

(defprotocol ComputeAdjacency
  (adjacency [this]
    "Returns the graph in adjacency form (map of predecessor to set of successors)."))

(defprotocol ComputeEdges
  (edges [this]
    "Returns the edges of the graph."))

(defprotocol ComputeEdgesOf
  (edges-of [this node]
    "Returns the edges of the given node."))

(defprotocol ComputeDegree
  (degree [this node]
    "Returns the number of edges of the given node."))

(defprotocol ComputeNeighbors
  (neighbors [this node]
    "Returns the neighboring nodes of the given node as a set."))

(defprotocol ComputeSources
  (sources [this]
    "Returns the source nodes of the graph."))

(defprotocol ComputeSinks
  (sinks [this]
    "Returns the sink nodes of the graph."))

(defprotocol ComputeProducers
  (producers [this]
    "Returns the nodes that have outbound edges."))

(defprotocol ComputeConsumers
  (consumers [this]
    "Returns the nodes that have inbound edges."))

(defprotocol ComputeInterior
  (interior [this]
    "Returns the nodes that have both inbound and outbound edges."))

(defprotocol ComputeInverse
  (inverse [this]
    "Returns a new graph containing reversed edges."))

(defprotocol ComputeTransitiveClosure
  (transitive-closure [this]
    "Returns a new graph containing additional edges expressing the transitive closure."))

(defprotocol ComputeComponents
  (components [this]
    "Returns the connected components of the graph."))

(defprotocol ComputeRoot
  (root [this]
    "Returns the root node of the graph."))

(defprotocol ComputeBranches
  (branches [this]
    "A tree oriented alias of `producers`"))

(defprotocol ComputeLeaves
  (leaves [this]
    "A tree oriented alias of `sinks`"))

(defprotocol ComputeParent
  (parent [this node]
    "Returns the parent of the node if there is only one parent, else throws an exception."))

(defprotocol ComputeChildren
  (children [this node]
    "A tree oriented alias of `successors`"))

(defprotocol ComputeAncestorsDepthFirst
  (ancestors-depth-first [this node]
    "Returns a lazy sequence of all the ancestors of the given node."))

(defprotocol ComputeAncestorsBreadthFirst
  (ancestors-breadth-first [this node]
    "Returns a lazy sequence of all the ancestors of the given node."))

(defprotocol ComputeDescendantsDepthFirst
  (descendants-depth-first [this node]
    "Returns a lazy sequence of all the descendants of the given node."))

(defprotocol ComputeDescendantsBreadthFirst
  (descendants-breadth-first [this node]
    "Returns a lazy sequence of all the descendants of the given node."))

(defprotocol ComputeOptimize
  (optimize [this]
    "Returns a new graph with the same semantics but optimized for performance."))

(defprotocol ComputeSupergraph
  (supergraph? [this other]
    "Returns true if the given graph is a supergraph of the other graph."))

(defprotocol ComputeSubgraph
  (subgraph? [this other]
    "Returns true if the given graph is a subgraph of the other graph."))

(defprotocol ComputeTopologicalSort
  (topological-sort [this]
    "Returns a lazy sequence of the nodes in topological order."))

(defprotocol ComputeWithNode
  (with-node [this node]
    "Returns a new graph with the node added."))

(defprotocol ComputeWithEdge
  (with-edge [this source sink]
    "Returns a new graph with the edge added."))

(defprotocol ComputeWithoutNode
  (without-node [this node]
    "Returns a new graph with the node removed."))

(defprotocol ComputeWithoutEdge
  (without-edge [this source sink]
    "Returns a new graph with the edge removed."))

(defprotocol ComputeFilterNodes
  (filter-nodes [this pred]
    "Returns a new graph with the nodes filtered according to pred."))

(defprotocol ComputeTransitivePreservingFilterNodes
  (transitive-preserving-filter-nodes [this pred]
    "Returns a new graph with the nodes filtered according to pred."))

(defprotocol ComputeMapNodes
  (map-nodes [this f]
    "Returns a new graph with the nodes transformed according to f."))

(defprotocol ComputeMapcatNodes
  (mapcat-nodes [this f]
    "Returns a new graph with the nodes transformed according to f."))

(defprotocol ComputeComplete
  (complete [this]
    "Returns a new graph with all possible edges added."))

(defprotocol ComputeComplement
  (complement [this]
    "Returns a new graph which contains the same set of nodes and all the
     edges which exist in the complete graph but not in the given graph."))

(defprotocol ComputeBidirectional
  (bidirectional [this]
    "Returns a new graph with all edges reversed and added to the graph."))

(defprotocol ComputeComponentSubgraph
  (component-subgraph [this node]
    "Returns the subgraph containing the given node and all of its ancestors and descendants."))

(defprotocol ComputeAncestorSubgraph
  (ancestors-subgraph [this node]
    "Returns the subgraph containing the given node and all of its ancestors."))

(defprotocol ComputeDescendantSubgraph
  (descendants-subgraph [this node]
    "Returns the subgraph containing the given node and all of its descendants"))

(defprotocol ComputeShortestPaths
  (shortest-paths [this weight-fn]
    "Computes a mapping of pairs of connected nodes to the shortest paths between them.
     Weight functions should accept a source node and a sink node and return some numerical
     cost to traverse the edge between them."))

