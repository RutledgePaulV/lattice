(ns io.github.rutledgepaulv.lattice.core
  "Functions for operating on graphs defined in terms of abstract protocols.

    The minimum requirement for these functions to work is that your graph type
    implements the `ComputedNodes` protocol and either the `ComputedSuccessors`
    or `ComputedPredecessors` protocols. A default implementation is included for
    maps in adjacency form."
  (:require [clojure.core.async :as async]
            [io.github.rutledgepaulv.lattice.protocols :as protos]
            [io.github.rutledgepaulv.lattice.process :as proc]
            [io.github.rutledgepaulv.lattice.utils :as utils]
            [io.github.rutledgepaulv.lattice.impls.abstract]
            [io.github.rutledgepaulv.lattice.impls.concrete])
  (:refer-clojure :exclude [reduce complement empty]))

(defn optimize
  "Returns a graph with the same semantics but that may be optimized
   (read: memoization, indexing, etc.) to provide better performance
   for algorithms."
  [graph]
  (protos/optimize graph))

(defn empty
  "Returns an abstract empty graph. {} would be fine too."
  []
  (utils/empty))

(defn eq
  "Returns true if graphs have the same nodes and edges."
  ([_] true)
  ([graph1 graph2]
   (protos/eq graph1 graph2))
  ([graph1 graph2 & graphs]
   (clojure.core/reduce
     (fn [result [prev-graph next-graph]]
       (if (protos/eq prev-graph next-graph)
         result
         (reduced false)))
     true
     (partition 2 1 (cons graph1 (cons graph2 graphs))))))

(defn sorted
  "Returns a graph upon which other calls will produce sorted collections."
  ([graph]
   (protos/sorted graph compare))
  ([graph comparator]
   (protos/sorted graph comparator)))

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
  (utils/union graphs))

(defn intersection
  "Returns the intersection of the given graphs."
  [& graphs]
  (utils/intersection graphs))

(defn difference
  "Returns the difference between the given graphs."
  [& graphs]
  (utils/difference graphs))

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

(defn components
  "Returns the connected component subgraphs of the given graph."
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

(defn exterior
  "Returns the set of nodes are either sources nor sinks."
  [graph]
  (protos/exterior graph))

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

(defn cycles
  "Returns a set of elementary cycles in the graph. An elementary cycle is a
   path which only contains distinct nodes and forms a cycle if you return to
   the first node once you reach the end. Every rotation of an elementary cycle
   is also a cycle."
  [graph]
  (protos/cycles graph))

(defn ancestors-depth-first
  "Returns a lazy sequence of all the ancestors of the given node."
  [graph node]
  (protos/ancestors-depth-first graph node))

(defn descendants-depth-first
  "Returns a lazy sequence of all the descendants of the given node."
  [graph node]
  (protos/descendants-depth-first graph node))

(defn ancestors-breadth-first
  "Returns a lazy sequence of all the ancestors of the given node."
  [graph node]
  (protos/ancestors-breadth-first graph node))

(defn descendants-breadth-first
  "Returns a lazy sequence of all the descendants of the given node."
  [graph node]
  (protos/descendants-breadth-first graph node))

(defn topological-sort
  "Returns the topological sort of the graph if it is a DAG, else nil. If this
   returns nil you can use io.github.rutledgepaulv.lattice.core/cycles to find
   the paths in the graph which make it cyclical and seek to eliminate them."
  [graph]
  (protos/topological-sort graph))

(defn with-node
  "Returns a new graph with the added node."
  [graph node]
  (protos/with-node graph node))

(defn with-edge
  "Returns a new graph with the added edge."
  [graph source sink]
  (protos/with-edge graph source sink))

(defn without-node
  "Returns a new graph without the node."
  [graph node]
  (protos/without-node graph node))

(defn without-edge
  "Returns a new graph without the edge."
  [graph source sink]
  (protos/without-edge graph source sink))

(defn filter-nodes
  "Returns a new graph containing only the nodes for which pred is truthy. Edges
  connected to those nodes are also removed. If you want to preserve the transitive
  closure of the graph, use `transitive-preserving-filter-nodes` instead."
  [graph pred]
  (protos/filter-nodes graph pred))

(defn remove-nodes
  "Returns a new graph without the nodes for which pred is truthy. Edges connected
   to those nodes are also removed. If you want to preserve the transitive closure
   of the graph, use `transitive-preserving-remove-nodes` instead."
  [graph pred]
  (protos/filter-nodes graph (clojure.core/complement pred)))

(defn transitive-preserving-filter-nodes
  "Returns a new graph containing only the nodes for which pred is truthy. Edges
   involving any removed nodes are also removed but new edges are added in their
   place to connect all predecessors and successors of the removed nodes."
  [graph pred]
  (protos/transitive-preserving-filter-nodes graph pred))

(defn transitive-preserving-remove-nodes
  "Returns a new graph without the nodes for which pred is truthy. Edges involving
   the removed nodes are removed but new edges are added in their place to connect
   all predecessors and successors of the removed nodes."
  [graph pred]
  (protos/transitive-preserving-filter-nodes graph (clojure.core/complement pred)))

(defn map-nodes
  "Returns a new graph with the nodes transformed according to f. Edges are retained
  between the new nodes and the transformations of the original predecessors
  and successors."
  [graph f]
  (protos/map-nodes graph f))

(defn mapcat-nodes
  "Returns a new graph where the nodes are replaced with zero or more nodes produced by applying f.
   Edges are retained between the new nodes and the transformations of the original predecessors
   and successors."
  [graph f]
  (protos/mapcat-nodes graph f))

(defn complete
  "Returns a new graph with all possible edges added."
  [graph]
  (protos/complete graph))

(defn complement
  "Returns a new graph which contains the same set of nodes and all the
   edges which exist in the complete graph but not in the given graph."
  [graph]
  (protos/complement graph))

(defn bidirectional
  "Returns a new graph with all edges reversed and added to the graph."
  [graph]
  (protos/bidirectional graph))

(defn component-subgraph
  "Returns the subgraph containing the given node and any nodes in the same connected component."
  [graph node]
  (protos/component-subgraph graph node))

(defn descendants-subgraph
  "Returns the subgraph containing the given node and all of its descendants."
  [graph node]
  (protos/descendants-subgraph graph node))

(defn ancestors-subgraph
  "Returns the subgraph containing the given node and all of its ancestors."
  [graph node]
  (protos/ancestors-subgraph graph node))

(defn shortest-paths
  "Returns a mapping of connected nodes to a map containing the distance and
  shortest path between those connected nodes. A weight-fn may be supplied to
  customize the cost of traversing each edge."
  ([graph]
   (shortest-paths graph (constantly 1)))
  ([graph weight-fn]
   (protos/shortest-paths graph weight-fn)))

(defn reduce
  "Returns the output channel which will receive maps containing the state of the reduction,
   the nodes that were visited, the nodes that were skipped, and the errors that were
   encountered. Reducer will run on the core.async dispatch thread pool and must return
   the new state after incorporating a node.

   Should be used for parallelism with computational tasks."
  ([graph reducer]
   (reduce graph reducer {}))
  ([graph reducer init]
   (let [output-chan (async/chan)]
     (reduce graph reducer init output-chan)
     (proc/final-chan output-chan)))
  ([graph reducer init output-chan]
   (reduce graph reducer init output-chan true))
  ([graph reducer init output-chan close?]
   (reduce graph reducer init output-chan close? utils/deep-merge))
  ([graph reducer init output-chan close? combiner]
   (proc/reduce-and-combine graph (proc/computational-applicator reducer) init output-chan close? combiner)))

(defn reduce-async
  "Returns the output channel which will receive maps containing the state of the reduction,
   the nodes that were visited, the nodes that were skipped, and the errors that were
   encountered. Reducer runs on the core.async dispatch thread pool and must return a
   single-use channel that will eventually produce the state of having incorporated the node.

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
   (reduce-async graph reducer init output-chan close? utils/deep-merge))
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
   (reduce-blocking graph reducer init output-chan close? utils/deep-merge))
  ([graph reducer init output-chan close? combiner]
   (proc/reduce-and-combine graph (proc/blocking-applicator reducer) init output-chan close? combiner)))
