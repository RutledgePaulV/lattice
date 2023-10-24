(ns io.github.rutledgepaulv.lattice.process
  (:require [clojure.core.async :as async]
            [io.github.rutledgepaulv.lattice.protocols :as protos]
            [clojure.set :as sets]))

(defn async-reduce-and-combine [graph init reducer combiner]
  (async/go-loop
    [result init
     visited #{}
     pending (reduce
               (fn [pending node]
                 (assoc pending node
                                (async/thread
                                  (try
                                    [true node (reducer init node)]
                                    (catch Exception e
                                      [false node e])))))
               {}
               (protos/sources graph))
     errors {}]
    (if (empty? pending)
      {:result  result
       :errors  errors
       :visited visited
       :skipped (sets/difference (protos/nodes graph) visited (set (keys errors)))}
      (let [[[success node value]] (async/alts! (vals pending))]
        (if success
          (let [result'  (combiner result value)
                visited' (conj visited node)]
            (recur
              result'
              visited'
              (reduce
                (fn [pending node]
                  (if (sets/subset? (protos/predecessors graph node) visited')
                    (assoc pending node
                                   (async/thread
                                     (try
                                       [true node (reducer result' node)]
                                       (catch Exception e
                                         [false node e]))))
                    pending))
                (dissoc pending node)
                (protos/successors graph node))
              errors))
          (recur result
                 visited
                 (dissoc pending node)
                 (assoc errors node value)))))))

(defn blocking-reduce-and-combine [graph init reducer combiner]
  (async/<!! (async-reduce-and-combine graph init reducer combiner)))
