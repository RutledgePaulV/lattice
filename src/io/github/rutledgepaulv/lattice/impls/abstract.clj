(ns io.github.rutledgepaulv.lattice.impls.abstract
  "Define all the graph operations in terms of other abstract protocols.
   These serve as the only implementations unless there's a concrete
   implementation defined elsewhere for performance reasons. Concrete
   implementations included with this library can be found in the
   io.github.rutledgepaulv.lattice.impls.concrete namespace."
  (:refer-clojure :exclude [ancestors complement descendants])
  (:require [clojure.set :as sets]
            [io.github.rutledgepaulv.lattice.utils :as combinators]
            [io.github.rutledgepaulv.lattice.protocols :as protos])
  (:import (io.github.rutledgepaulv.lattice.protocols Graph)
           (java.io Writer)))

(extend-protocol protos/ComputeSuccessors
  Object
  (successors [this node]
    (into #{} (filter (fn [other] (contains? (protos/predecessors this other) node))) (protos/nodes this))))

(extend-protocol protos/ComputePredecessors
  Object
  (predecessors [this node]
    (into #{} (filter (fn [other] (contains? (protos/successors this other) node))) (protos/nodes this))))

(extend-protocol protos/ComputeInboundEdges
  Object
  (inbound-edges [this node]
    (set (map vector (protos/predecessors this node) (repeat node)))))

(extend-protocol protos/ComputeOutboundEdges
  Object
  (outbound-edges [this node]
    (set (map vector (repeat node) (protos/successors this node)))))

(extend-protocol protos/ComputeAdjacency
  Object
  (adjacency [this]
    (persistent!
      (reduce
        (fn [adjacency node]
          (assoc! adjacency node (protos/successors this node)))
        (transient {})
        (protos/nodes this)))))

(extend-protocol protos/ComputeEdges
  Object
  (edges [this]
    (reduce
      (fn [edges node]
        (sets/union edges (protos/outbound-edges this node)))
      #{}
      (protos/nodes this))))

(extend-protocol protos/ComputeEdgesOf
  Object
  (edges-of [this node]
    (sets/union
      (protos/inbound-edges this node)
      (protos/outbound-edges this node))))

(extend-protocol protos/ComputeDegree
  Object
  (degree [this node]
    (count (protos/edges-of this node))))

(extend-protocol protos/ComputeNeighbors
  Object
  (neighbors [this node]
    (sets/union
      (protos/successors this node)
      (protos/predecessors this node))))

(extend-protocol protos/ComputeSources
  Object
  (sources [this]
    (into #{} (filter (fn [node] (empty? (protos/predecessors this node)))) (protos/nodes this))))

(extend-protocol protos/ComputeSinks
  Object
  (sinks [this]
    (into #{} (filter (fn [node] (empty? (protos/successors this node)))) (protos/nodes this))))

(extend-protocol protos/ComputeProducers
  Object
  (producers [this]
    (into #{} (remove (fn [node] (empty? (protos/successors this node)))) (protos/nodes this))))

(extend-protocol protos/ComputeConsumers
  Object
  (consumers [this]
    (into #{} (remove (fn [node] (empty? (protos/predecessors this node)))) (protos/nodes this))))

(extend-protocol protos/ComputeInterior
  Object
  (interior [this]
    (into #{}
          (remove
            (fn [node]
              (or (empty? (protos/successors this node))
                  (empty? (protos/predecessors this node)))))
          (protos/nodes this))))

(extend-protocol protos/ComputeExterior
  Object
  (exterior [this]
    (sets/union (protos/sinks this) (protos/sources this))))

(extend-protocol protos/ComputeInverse
  Object
  (inverse [this]
    (reify
      Graph
      protos/ComputeNodes
      (nodes [_]
        (protos/nodes this))
      protos/ComputeSuccessors
      (successors [_ node]
        (protos/predecessors this node))
      protos/ComputePredecessors
      (predecessors [_ node]
        (protos/successors this node)))))

(extend-protocol protos/ComputeDescendantsDepthFirst
  Object
  (descendants-depth-first [this node]
    (let [memo (memoize (fn [node] (protos/successors this node)))]
      (cond->> (combinators/graph-seq-depth-first (comp not-empty memo) memo node)
               (contains? (memo node) node)
               (cons node)))))

(extend-protocol protos/ComputeAncestorsDepthFirst
  Object
  (ancestors-depth-first [this node]
    (let [memo (memoize (fn [node] (protos/predecessors this node)))]
      (cond->> (combinators/graph-seq-depth-first (comp not-empty memo) memo node)
               (contains? (memo node) node)
               (cons node)))))

(extend-protocol protos/ComputeDescendantsBreadthFirst
  Object
  (descendants-breadth-first [this node]
    (let [memo (memoize (fn [node] (protos/successors this node)))]
      (cond->> (combinators/graph-seq-breadth-first (comp not-empty memo) memo node)
               (contains? (memo node) node)
               (cons node)))))

(extend-protocol protos/ComputeAncestorsBreadthFirst
  Object
  (ancestors-breadth-first [this node]
    (let [memo (memoize (fn [node] (protos/predecessors this node)))]
      (cond->> (combinators/graph-seq-breadth-first (comp not-empty memo) memo node)
               (contains? (memo node) node)
               (cons node)))))

(extend-protocol protos/ComputeComponentSubgraph
  Object
  (component-subgraph [this node]
    (let [nodes
          (-> this
              (protos/bidirectional)
              (protos/transitive-closure)
              (protos/successors node)
              (conj node)
              (delay))]
      (reify
        Graph
        protos/ComputeNodes
        (nodes [_]
          (force nodes))
        protos/ComputeSuccessors
        (successors [_ node]
          (sets/intersection (force nodes) (protos/successors this node)))
        protos/ComputePredecessors
        (predecessors [_ node]
          (sets/intersection (force nodes) (protos/predecessors this node)))))))

(extend-protocol protos/ComputeDescendantSubgraph
  Object
  (descendants-subgraph [this node]
    (if (contains? (protos/nodes this) node)
      (let [nodes (delay (conj (set (protos/descendants-depth-first this node)) node))]
        (reify
          Graph
          protos/ComputeNodes
          (nodes [_]
            (force nodes))
          protos/ComputeSuccessors
          (successors [_ node]
            (sets/intersection (force nodes) (protos/successors this node)))
          protos/ComputePredecessors
          (predecessors [_ node]
            (sets/intersection (force nodes) (protos/predecessors this node)))))
      (combinators/empty))))

(extend-protocol protos/ComputeAncestorSubgraph
  Object
  (ancestors-subgraph [this node]
    (if (contains? (protos/nodes this) node)
      (let [nodes (delay (conj (set (protos/ancestors-depth-first this node)) node))]
        (reify
          Graph
          protos/ComputeNodes
          (nodes [_]
            (force nodes))
          protos/ComputeSuccessors
          (successors [_ node]
            (sets/intersection (force nodes) (protos/successors this node)))
          protos/ComputePredecessors
          (predecessors [_ node]
            (sets/intersection (force nodes) (protos/predecessors this node)))))
      (combinators/empty))))

(extend-protocol protos/ComputeRoot
  Object
  (root [this]
    (first (protos/sources this))))

(extend-protocol protos/ComputeBranches
  Object
  (branches [this]
    (protos/producers this)))

(extend-protocol protos/ComputeLeaves
  Object
  (leaves [this]
    (protos/sinks this)))

(extend-protocol protos/ComputeParent
  Object
  (parent [this node]
    (first (protos/predecessors this node))))

(extend-protocol protos/ComputeChildren
  Object
  (children [this node]
    (protos/successors this node)))

(extend-protocol protos/ComputeOptimize
  Object
  (optimize [this]
    (let [memoized-nodes        (memoize (fn [] (protos/nodes this)))
          memoized-successors   (memoize (fn [node] (protos/successors this node)))
          memoized-predecessors (memoize (fn [node] (protos/predecessors this node)))
          memoized-adjacency    (memoize (fn [] (protos/adjacency this)))]
      (reify
        Graph
        protos/ComputeNodes
        (nodes [_]
          (memoized-nodes))
        protos/ComputeSuccessors
        (successors [_ node]
          (memoized-successors node))
        protos/ComputePredecessors
        (predecessors [_ node]
          (memoized-predecessors node))
        protos/ComputeAdjacency
        (adjacency [_]
          (memoized-adjacency))))))

(extend-protocol protos/ComputeSupergraph
  Object
  (supergraph? [this other]
    (and
      (sets/superset? (protos/nodes this) (protos/nodes other))
      (sets/superset? (protos/edges this) (protos/edges other)))))

(extend-protocol protos/ComputeSubgraph
  Object
  (subgraph? [this other]
    (and
      (sets/subset? (protos/nodes this) (protos/nodes other))
      (sets/subset? (protos/edges this) (protos/edges other)))))

(extend-protocol protos/ComputeWithNode
  Object
  (with-node [this node-to-add]
    (reify
      Graph
      protos/ComputeNodes
      (nodes [_]
        (conj (protos/nodes this) node-to-add))
      protos/ComputeSuccessors
      (successors [_ node]
        (protos/successors this node))
      protos/ComputePredecessors
      (predecessors [_ node]
        (protos/predecessors this node)))))

(extend-protocol protos/ComputeWithEdge
  Object
  (with-edge [this source sink]
    (reify
      Graph
      protos/ComputeNodes
      (nodes [_]
        (conj (protos/nodes this) source sink))
      protos/ComputeSuccessors
      (successors [_ node]
        (if (= node source)
          (conj (protos/successors this node) sink)
          (protos/successors this node)))
      protos/ComputePredecessors
      (predecessors [_ node]
        (if (= node sink)
          (conj (protos/predecessors this node) source)
          (protos/predecessors this node))))))

(extend-protocol protos/ComputeWithoutNode
  Object
  (without-node [this node-to-remove]
    (reify
      Graph
      protos/ComputeNodes
      (nodes [_]
        (disj (protos/nodes this) node-to-remove))
      protos/ComputeSuccessors
      (successors [_ node]
        (if (= node node-to-remove)
          #{}
          (disj (protos/successors this node) node-to-remove)))
      protos/ComputePredecessors
      (predecessors [_ node]
        (if (= node node-to-remove)
          #{}
          (disj (protos/predecessors this node) node-to-remove))))))

(extend-protocol protos/ComputeWithoutEdge
  Object
  (without-edge [this source sink]
    (reify
      Graph
      protos/ComputeNodes
      (nodes [_]
        (protos/nodes this))
      protos/ComputeSuccessors
      (successors [_ node]
        (if (= node source)
          (disj (protos/successors this node) sink)
          (protos/successors this node)))
      protos/ComputePredecessors
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

(extend-protocol protos/ComputeMapNodes
  Object
  (map-nodes [this f]
    (protos/mapcat-nodes this (fn [node] #{(f node)}))))

(extend-protocol protos/ComputeFilterNodes
  Object
  (filter-nodes [this pred]
    (let [memoized (memoize pred)]
      (reify
        Graph
        protos/ComputeNodes
        (nodes [_]
          (into #{} (filter memoized) (protos/nodes this)))
        protos/ComputeSuccessors
        (successors [_ node]
          (if (memoized node)
            (into #{} (filter memoized) (protos/successors this node))
            #{}))
        protos/ComputePredecessors
        (predecessors [_ node]
          (if (memoized node)
            (into #{} (filter memoized) (protos/predecessors this node))
            #{}))))))

(extend-protocol protos/ComputeMapcatNodes
  Object
  (mapcat-nodes [this f]
    (let [it (delay
               (reduce
                 (fn [agg old-node]
                   (reduce
                     (fn [agg new-node]
                       (-> agg
                           (update-in [:-> old-node] (fnil conj #{}) new-node)
                           (update-in [:<- new-node] (fnil conj #{}) old-node)))
                     (update-in agg [:-> old-node] #(or % #{}))
                     (f old-node)))
                 {:-> {}
                  :<- {}}
                 (protos/nodes this)))]
      (reify
        Graph
        protos/ComputeNodes
        (nodes [_]
          (set (keys (:<- (force it)))))
        protos/ComputeSuccessors
        (successors [_ node]
          (let [old-nodes (get-in (force it) [:<- node] #{})]
            (into #{}
                  (comp
                    (mapcat (fn [node] (protos/successors this node)))
                    (mapcat (fn transform [old-node]
                              (let [new-successors (get-in (force it) [:-> old-node] #{})]
                                (if (empty? new-successors)
                                  (mapcat transform (protos/successors this old-node))
                                  new-successors)))))
                  old-nodes)))
        protos/ComputePredecessors
        (predecessors [_ node]
          (let [old-nodes (get-in (force it) [:<- node] #{})]
            (into #{}
                  (comp
                    (mapcat (fn [node] (protos/predecessors this node)))
                    (mapcat (fn transform [old-node]
                              (let [new-predecessors (get-in (force it) [:-> old-node] #{})]
                                (if (empty? new-predecessors)
                                  (mapcat transform (protos/predecessors this old-node))
                                  new-predecessors)))))
                  old-nodes)))))))

(extend-protocol protos/ComputeTransitivePreservingFilterNodes
  Object
  (transitive-preserving-filter-nodes [this pred]
    (protos/mapcat-nodes this (fn [node] (if (pred node) #{node} #{})))))

(extend-protocol protos/ComputeTransitiveClosure
  Object
  (transitive-closure [this]
    (reify
      Graph
      protos/ComputeNodes
      (nodes [_]
        (protos/nodes this))
      protos/ComputeSuccessors
      (successors [_ node]
        (into #{} (protos/descendants-depth-first this node)))
      protos/ComputePredecessors
      (predecessors [_ node]
        (into #{} (protos/ancestors-depth-first this node))))))

(extend-protocol protos/ComputeComplete
  Object
  (complete [this]
    (let [nodes (delay (protos/nodes this))]
      (reify
        Graph
        protos/ComputeNodes
        (nodes [_]
          (force nodes))
        protos/ComputeSuccessors
        (successors [_ node]
          (let [the-nodes (force nodes)]
            (if (contains? the-nodes node)
              (disj the-nodes node)
              #{})))
        protos/ComputePredecessors
        (predecessors [_ node]
          (let [the-nodes (force nodes)]
            (if (contains? the-nodes node)
              (disj the-nodes node)
              #{})))))))

(extend-protocol protos/ComputeComplement
  Object
  (complement [this]
    (let [nodes (delay (protos/nodes this))]
      (reify
        Graph
        protos/ComputeNodes
        (nodes [_]
          (force nodes))
        protos/ComputeSuccessors
        (successors [_ node]
          (let [the-nodes (force nodes)]
            (if (contains? the-nodes node)
              (sets/difference
                (disj the-nodes node)
                (protos/successors this node))
              #{})))
        protos/ComputePredecessors
        (predecessors [_ node]
          (let [the-nodes (force nodes)]
            (if (contains? the-nodes node)
              (sets/difference
                (disj the-nodes node)
                (protos/predecessors this node))
              #{})))))))

(extend-protocol protos/ComputeBidirectional
  Object
  (bidirectional [this]
    (reify
      Graph
      protos/ComputeNodes
      (nodes [_]
        (protos/nodes this))
      protos/ComputeSuccessors
      (successors [_ node]
        (sets/union (protos/successors this node) (protos/predecessors this node)))
      protos/ComputePredecessors
      (predecessors [_ node]
        (sets/union (protos/successors this node) (protos/predecessors this node))))))

(extend-protocol protos/ComputeComponents
  Object
  (components [this]
    (:results
      (let [closure (protos/transitive-closure (protos/bidirectional this))]
        (reduce
          (fn [{:keys [seen] :as agg} node]
            (if (contains? seen node)
              agg
              (let [component (conj (protos/successors closure node) node)]
                (-> agg
                    (update :results conj component)
                    (update :seen sets/union component)))))
          {:results #{} :seen #{}}
          (protos/nodes this))))))

(extend-protocol protos/ComputeShortestPaths
  Object
  (shortest-paths [this weight-fn]
    (let [ns        (protos/nodes this)
          weight-fn (or weight-fn (constantly 1))
          es        (protos/edges this)
          {:keys [dist next]}
          (as-> {:dist {} :next {}} reduction
                (reduce
                  (fn [{:keys [dist next]} [u v]]
                    (cond
                      (contains? es [u v])
                      {:dist (assoc dist [u v] (weight-fn u v))
                       :next (assoc next [u v] v)}

                      (= u v)
                      {:dist (assoc dist [v v] 0)
                       :next (assoc next [v v] v)}

                      :otherwise
                      {:dist (assoc dist [u v] Double/POSITIVE_INFINITY)
                       :next (assoc next [u v] nil)}))
                  reduction
                  (for [i ns j ns] [i j]))
                (reduce
                  (fn [{:keys [dist next] :as agg} [i j k]]
                    (let [candidate
                          (+ (get dist [i k] Double/POSITIVE_INFINITY)
                             (get dist [k j] Double/POSITIVE_INFINITY))]
                      (if (> (get dist [i j] Double/POSITIVE_INFINITY) candidate)
                        {:dist (assoc dist [i j] candidate)
                         :next (assoc next [i j] (get next [i k]))}
                        agg)))
                  reduction
                  (for [k ns i ns j ns] [i j k])))]
      (->> (for [u ns v ns]
             [[u v] (if (get next [u v])
                      (loop [u u path [u]]
                        (if-not (= u v)
                          (let [new-u (get next [u v])]
                            (recur new-u (conj path new-u)))
                          path))
                      [])])
           (remove (comp empty? second))
           (map (fn [[k v]] [k {:distance (get dist k) :path v}]))
           (into {})))))

(extend-protocol protos/ComputeEquals
  Object
  (eq [this other]
    (and
      (= (protos/nodes this) (protos/nodes other))
      (= (protos/edges this) (protos/edges other)))))

(extend-protocol protos/ComputeSorted
  Object
  (sorted [this comparator]
    (reify Graph
      protos/ComputeNodes
      (nodes [_]
        (into (sorted-set-by comparator) (protos/nodes this)))
      protos/ComputeSuccessors
      (successors [_ node]
        (into (sorted-set-by comparator) (protos/successors this node)))
      protos/ComputePredecessors
      (predecessors [_ node]
        (into (sorted-set-by comparator) (protos/predecessors this node)))
      protos/ComputeAdjacency
      (adjacency [self]
        (reduce
          (fn [adjacency node]
            (assoc adjacency node (protos/successors self node)))
          (sorted-map-by comparator)
          (protos/nodes self))))))


(extend-protocol protos/ComputeCycles
  Object
  ; johnson's algorithm for finding all elementary cycles ported from python's networkx
  ; https://networkx.org/documentation/networkx-2.4/_modules/networkx/algorithms/cycles.html#simple_cycles
  (cycles [this]
    (loop [components (protos/components this) cycles #{}]
      (if (empty? components)
        cycles
        (recur (rest components)
               (let [component  (protos/filter-nodes this (first components))
                     start-node (first (protos/nodes component))]
                 (loop [cycles  cycles
                        path    [start-node]
                        blocked #{start-node}
                        closed  #{}
                        B       {}
                        stack   [[start-node (protos/successors this start-node)]]]
                   (if (empty? stack)
                     cycles
                     (let [[this-node successors] (peek stack)]
                       (if (not-empty successors)
                         (let [next-node (first successors)]
                           (cond
                             (= next-node start-node)
                             (recur (conj cycles path) path blocked (into closed path) B (conj (pop stack) [this-node (disj successors next-node)]))
                             (not (contains? blocked next-node))
                             (recur cycles
                                    (conj path next-node)
                                    (conj blocked next-node)
                                    (disj closed next-node)
                                    B
                                    (-> (pop stack)
                                        (conj [this-node (disj successors next-node)])
                                        (conj [next-node (protos/successors this next-node)])))))
                         (if (contains? closed this-node)
                           (let [[blocked B]
                                 (loop [stack   #{this-node}
                                        blocked blocked
                                        B       B]
                                   (if (empty? stack)
                                     [blocked B]
                                     (let [node (first stack)]
                                       (recur
                                         (disj (sets/union stack (get B node #{})) node)
                                         (disj blocked node)
                                         (dissoc B node)))))]
                             (recur cycles (pop path) blocked closed B (pop stack)))
                           (let [B' (reduce
                                      (fn [b' successor]
                                        (if (contains? (protos/successors this successor) this-node)
                                          b'
                                          (update b' successor (fnil conj #{}) this-node)))
                                      B
                                      successors)]
                             (recur cycles (pop path) blocked closed B' (pop stack))))))))))))))

(defmethod print-dup Graph [obj ^Writer writer]
  (print-dup (protos/adjacency obj) writer))

(defmethod print-method Graph [obj ^Writer writer]
  (print-method (protos/adjacency obj) writer))
