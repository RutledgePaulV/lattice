(ns io.github.rutledgepaulv.lattice.core
  "Functions for operating on graphs defined in terms of abstract protocols.

    The minimum requirement for these functions to work is that your graph type
    implements the `ComputedNodes` protocol and either the `ComputedSuccessors`
    or `ComputedPredecessors` protocols. A default implementation is included for
    maps in adjacency form."
  (:require [clojure.core.async :as async]
            [io.github.rutledgepaulv.lattice.protocols :as protos]
            [io.github.rutledgepaulv.lattice.process :as proc]
            [io.github.rutledgepaulv.lattice.combinators :as combinators]
            [io.github.rutledgepaulv.lattice.impls.abstract]
            [io.github.rutledgepaulv.lattice.impls.concrete])
  (:refer-clojure :exclude [ancestors descendants reduce]))

(defn optimize
  "Returns a graph with the same semantics but that may be optimized
   (read: memoization, indexing, etc.) to provide better performance
   for algorithms."
  [graph]
  (protos/optimize graph))

(defn nodes
  "Returns the nodes of the graph as a set."
  [graph]
  (protos/nodes graph))

(defn edges
  "Returns the edges of the graph, or the edges of a specific node."
  ([graph]
   (protos/edges graph))
  ([graph node]
   (protos/edges-of graph node)))

(defn source
  "Returns the source of an edge."
  [edge]
  (protos/source edge))

(defn sink
  "Returns the sink of an edge."
  [edge]
  (protos/sink edge))

(defn successors
  "Returns the nodes that follow the given node."
  [graph node]
  (protos/successors graph node))

(defn predecessors
  "Returns the nodes that precede the given node."
  [graph node]
  (protos/predecessors graph node))

(defn inbound-edges
  "Returns the inbound edges of the given node."
  [graph node]
  (protos/inbound-edges graph node))

(defn outbound-edges
  "Returns the outbound edges of the given node."
  [graph node]
  (protos/outbound-edges graph node))

(defn adjacency
  "Returns the graph in adjacency form (map of predecessor to set of successors)."
  [graph]
  (protos/adjacency graph))

(defn degree
  "Returns the number of edges of the given node."
  [graph node]
  (protos/degree graph node))

(defn neighbors
  "Returns the neighboring nodes of the given node as a set."
  [graph node]
  (protos/neighbors graph node))

(defn union
  "Returns the union of the given graphs."
  [& graphs]
  (combinators/union graphs))

(defn intersection
  "Returns the intersection of the given graphs."
  [& graphs]
  (combinators/intersection graphs))

(defn difference
  "Returns the difference between the given graphs."
  [& graphs]
  (combinators/difference graphs))

(defn subgraph?
  "Is graph1 a subgraph of graph2?"
  [graph1 graph2]
  (protos/subgraph? graph1 graph2))

(defn supergraph?
  "Is graph1 a supergraph of graph2?"
  [graph1 graph2]
  (protos/supergraph? graph1 graph2))

(defn inverse
  "Returns a new graph with all edges reversed from the given graph."
  [graph]
  (protos/inverse graph))

(defn transitive-closure
  "Returns a new graph containing added edges expressing the transitive closure of the given graph."
  [graph]
  (protos/transitive-closure graph))

(defn bridges
  "Returns the edges whose removal would alter the transitive closure of the graph."
  [graph]
  (protos/bridges graph))

(defn components
  "Returns the connected components in the given graph."
  [graph]
  (protos/components graph))

(defn sources
  "Returns the set of nodes that have no predecessors."
  [graph]
  (protos/sources graph))

(defn sinks
  "Returns the set of nodes that have no successors."
  [graph]
  (protos/sinks graph))

(defn producers
  "Returns the set of nodes that have successors."
  [graph]
  (protos/producers graph))

(defn consumers
  "Returns the set of nodes that have predecessors."
  [graph]
  (protos/consumers graph))

(defn interior
  "Returns the set of nodes are neither sources nor sinks."
  [graph]
  (protos/interior graph))

(defn root
  "Returns the root node of the tree if the graph is also a tree, else nil."
  [graph]
  (protos/root graph))

(defn branches
  "A tree oriented alias of `producers`"
  [graph]
  (protos/branches graph))

(defn leaves
  "A tree oriented alias of `sinks`"
  [graph]
  (protos/leaves graph))

(defn parent
  "Returns the parent of the node if there is only one parent, else throws an exception."
  [graph node]
  (protos/parent graph node))

(defn children
  "A tree oriented alias of `successors`"
  [graph node]
  (protos/children graph node))

(defn ancestors
  "Returns a lazy sequence of all the ancestors of the given node."
  [graph node]
  (protos/ancestors graph node))

(defn descendants
  "Returns a lazy sequence of all the descendants of the given node."
  [graph node]
  (protos/descendants graph node))

(defn reduce
  "Returns the output channel which will receive maps containing the state of the reduction,
   the nodes that were visited, the nodes that were skipped, and the errors that were
   encountered. Reducer will run on the core.async dispatch thread pool and must return
   the new state after incorporating a node.

   Should be used for computation parallelism."
  ([graph reducer]
   (reduce graph reducer {}))
  ([graph reducer init]
   (let [output-chan (async/chan)]
     (reduce graph reducer init output-chan)
     (proc/final-chan output-chan)))
  ([graph reducer init output-chan]
   (reduce graph reducer init output-chan true))
  ([graph reducer init output-chan close?]
   (reduce graph reducer init output-chan close? combinators/deep-merge))
  ([graph reducer init output-chan close? combiner]
   (proc/reduce-and-combine graph (proc/computational-applicator reducer) init output-chan close? combiner)))

(defn reduce-async
  "Returns the output channel which will receive maps containing the state of the reduction,
   the nodes that were visited, the nodes that were skipped, and the errors that were
   encountered. Reducer runs on the core.async dispatch thread pool and must return a
   channel that will eventually produce the state of having incorporated the node.

   Should be used for parallelism with non-blocking tasks."
  ([graph reducer]
   (reduce-async graph reducer {}))
  ([graph reducer init]
   (let [output-chan (async/chan)]
     (reduce-async graph reducer init output-chan)
     (proc/final-chan output-chan)))
  ([graph reducer init output-chan]
   (reduce-async graph reducer init output-chan true))
  ([graph reducer init output-chan close?]
   (reduce-async graph reducer init output-chan close? combinators/deep-merge))
  ([graph reducer init output-chan close? combiner]
   (proc/reduce-and-combine graph (proc/async-applicator reducer) init output-chan close? combiner)))

(defn reduce-blocking
  "Returns the output channel which will receive maps containing the state of the reduction,
   the nodes that were visited, the nodes that were skipped, and the errors that were
   encountered. Reducer runs on dedicated thread and should return the new state after
   incorporating a node.

   Should be used for parallelism with blocking tasks."
  ([graph reducer]
   (reduce-blocking graph reducer {}))
  ([graph reducer init]
   (let [output-chan (async/chan)]
     (reduce-blocking graph reducer init output-chan)
     (proc/final-chan output-chan)))
  ([graph reducer init output-chan]
   (reduce-blocking graph reducer init output-chan true))
  ([graph reducer init output-chan close?]
   (reduce-blocking graph reducer init output-chan close? combinators/deep-merge))
  ([graph reducer init output-chan close? combiner]
   (proc/reduce-and-combine graph (proc/blocking-applicator reducer) init output-chan close? combiner)))