(ns io.github.rutledgepaulv.lattice.impls.concrete
  "Type specific implementations bundled with the library. Users may
   of course choose to implement their own types. The minimum required
   is to implement `nodes` and either `successors` or `predecessors`."
  (:require [clojure.set :as sets]
            [io.github.rutledgepaulv.lattice.protocols :as protos])
  (:import (clojure.lang IPersistentMap IPersistentVector MapEntry)))


(extend-protocol protos/Edge
  IPersistentVector
  (source [this] (first this))
  (sink [this] (peek this))
  MapEntry
  (source [this] (key this))
  (sink [this] (val this)))

(extend-type IPersistentMap

  protos/ComputedNodes
  (nodes [this]
    (sets/union (set (keys this)) (set (mapcat identity (vals this)))))

  protos/ComputedSuccessors
  (successors [this node]
    (set (get this node #{})))

  protos/ComputedAdjacency
  (adjacency [this]
    this)

  protos/ComputedInverse
  (inverse [this]
    (let [reversed
          (reduce-kv
            (fn [agg source sinks]
              (reduce (fn [agg sink]
                        (-> agg
                            (update-in [:result sink] (fnil conj #{}) source)
                            (update :nodes conj sink source)))
                      (-> agg
                          (update-in [:result source] #(or % #{}))
                          (update :nodes conj source))
                      sinks))
            {:result {} :nodes #{}}
            this)]
      (reify
        protos/ComputedNodes
        (nodes [_]
          (:nodes reversed))
        protos/ComputedSuccessors
        (successors [_ node]
          (set (get-in reversed [:result node] #{})))
        protos/ComputedPredecessors
        (predecessors [_ node]
          (set (get this node #{}))))))

  protos/ComputedOptimize
  (optimize [this]
    (let [reversed (delay (protos/inverse this))]
      (reify
        protos/ComputedNodes
        (nodes [_]
          (protos/nodes (force reversed)))
        protos/ComputedSuccessors
        (successors [_ node]
          (protos/successors this node))
        protos/ComputedPredecessors
        (predecessors [_ node]
          (protos/successors (force reversed) node))))))
