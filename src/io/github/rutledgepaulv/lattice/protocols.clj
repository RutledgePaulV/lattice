(ns io.github.rutledgepaulv.lattice.protocols
  "Protocols for defining a graph. They are intentionally
   very granular so that implementations can be as efficient
   as possible. Default implementations are defined for Object
   (assumed to be something that implements at least `nodes` and
   one of `successors` or `predecessors`). Default implementations
   of `successors` is also defined for IPersistentMap."
  (:refer-clojure :exclude [ancestors descendants]))

(defprotocol Edge
  "A protocol for defining an edge."
  (source [this]
    "Returns the source node of the edge.")
  (sink [this]
    "Returns the sink node of the edge."))

(defprotocol ComputedNodes
  (nodes [this]
    "Returns the nodes of the graph as a set."))

(defprotocol ComputedSuccessors
  (successors [this node]
    "Returns the nodes that follow the given node."))

(defprotocol ComputedPredecessors
  (predecessors [this node]
    "Returns the nodes that precede the given node."))

(defprotocol ComputedInboundEdges
  (inbound-edges [this node]
    "Returns the inbound edges of the given node."))

(defprotocol ComputedOutboundEdges
  (outbound-edges [this node]
    "Returns the outbound edges of the given node."))

(defprotocol ComputedAdjacency
  (adjacency [this]
    "Returns the graph in adjacency form (map of predecessor to set of successors)."))

(defprotocol ComputedEdges
  (edges [this]
    "Returns the edges of the graph."))

(defprotocol ComputedEdgesOf
  (edges-of [this node]
    "Returns the edges of the given node."))

(defprotocol ComputedDegree
  (degree [this node]
    "Returns the number of edges of the given node."))

(defprotocol ComputedNeighbors
  (neighbors [this node]
    "Returns the neighboring nodes of the given node as a set."))

(defprotocol ComputedSources
  (sources [this]
    "Returns the source nodes of the graph."))

(defprotocol ComputedSinks
  (sinks [this]
    "Returns the sink nodes of the graph."))

(defprotocol ComputedProducers
  (producers [this]
    "Returns the nodes that have outbound edges."))

(defprotocol ComputedConsumers
  (consumers [this]
    "Returns the nodes that have inbound edges."))

(defprotocol ComputedInterior
  (interior [this]
    "Returns the nodes that have both inbound and outbound edges."))

(defprotocol ComputedInverse
  (inverse [this]
    "Returns a new graph containing reversed edges."))

(defprotocol ComputedTransitiveClosure
  (transitive-closure [this]
    "Returns a new graph containing additional edges expressing the transitive closure."))

(defprotocol ComputedBridges
  (bridges [this]
    "Returns the edges that if removed would alter the transitive closure."))

(defprotocol ComputedComponents
  (components [this]
    "Returns the connected components of the graph."))

(defprotocol ComputedRoot
  (root [this]
    "Returns the root node of the graph."))

(defprotocol ComputedBranches
  (branches [this]
    "A tree oriented alias of `producers`"))

(defprotocol ComputedLeaves
  (leaves [this]
    "A tree oriented alias of `sinks`"))

(defprotocol ComputedParent
  (parent [this node]
    "Returns the parent of the node if there is only one parent, else throws an exception."))

(defprotocol ComputedChildren
  (children [this node]
    "A tree oriented alias of `successors`"))

(defprotocol ComputedAncestors
  (ancestors [this node]
    "Returns a lazy sequence of all the ancestors of the given node."))

(defprotocol ComputedDescendants
  (descendants [this node]
    "Returns a lazy sequence of all the descendants of the given node."))

(defprotocol ComputedOptimize
  (optimize [this]
    "Returns a new graph with the same semantics but optimized for performance."))

(defprotocol ComputedSupergraph
  (supergraph? [this other]
    "Returns true if the given graph is a supergraph of the other graph."))

(defprotocol ComputedSubgraph
  (subgraph? [this other]
    "Returns true if the given graph is a subgraph of the other graph."))
