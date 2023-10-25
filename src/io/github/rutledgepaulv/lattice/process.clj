(ns io.github.rutledgepaulv.lattice.process
  (:require [clojure.core.async :as async]
            [io.github.rutledgepaulv.lattice.protocols :as protos]
            [clojure.set :as sets]))

(defn blocking-applicator [reducer]
  (fn [state node]
    (async/thread
      (try
        [true node (reducer state node)]
        (catch Exception e
          [false node e])))))

(defn computational-applicator [reducer]
  (fn [state node]
    (async/go
      (try
        [true node (reducer state node)]
        (catch Exception e
          [false node e])))))

(defn async-applicator [reducer]
  (fn [state node]
    (async/go
      (try
        (let [chan (reducer state node)]
          (try
            [true node (async/<! chan)]
            (finally
              (async/close! chan))))
        (catch Exception e
          [false node e])))))

(defn reduce-and-combine [graph reducer init output-chan close? combiner]
  (async/go-loop
    [result init
     visited #{}
     pending (reduce
               (fn [pending node]
                 (assoc pending node (reducer init node)))
               {}
               (protos/sources graph))
     errors {}]
    (let [[[success node value]] (async/alts! (vals pending))]
      (if success
        (let [result'  (combiner result value)
              visited' (conj visited node)]
          (if (async/>!
                output-chan
                {:result  result'
                 :errors  errors
                 :visited visited'
                 :pending (sets/difference (protos/nodes graph) visited' (set (keys errors)))})
            (let [pending' (reduce
                             (fn [pending node]
                               (if (sets/subset? (protos/predecessors graph node) visited')
                                 (assoc pending node (reducer result' node))
                                 pending))
                             (dissoc pending node)
                             (protos/successors graph node))]
              (if (not-empty pending')
                (recur result' visited' pending' errors)
                (when close? (async/close! output-chan))))
            (doseq [[_ chan] pending]
              (async/close! chan))))
        (let [pending' (dissoc pending node)
              errors'  (assoc errors node value)]
          (if (async/>!
                output-chan
                {:result  result
                 :errors  errors'
                 :visited visited
                 :pending (sets/difference (protos/nodes graph) visited (set (keys errors)))})
            (if (not-empty pending')
              (recur result visited pending' errors')
              (when close? (async/close! output-chan)))
            (doseq [[_ chan] pending']
              (async/close! chan)))))))
  output-chan)

(defn final-chan [chan]
  (async/go-loop [prev {}]
    (let [next (async/<! chan)]
      (if (some? next) (recur next) prev))))