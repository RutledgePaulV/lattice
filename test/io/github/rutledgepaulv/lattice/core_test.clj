(ns io.github.rutledgepaulv.lattice.core-test
  (:require [clojure.set :as sets]
            [clojure.test :refer :all]
            [io.github.rutledgepaulv.lattice.core :as lattice])
  (:refer-clojure :exclude [ancestors descendants]))

(defn random-graph []
  (let [nodes (random-sample (rand) (map keyword (map (comp str char) (range 97 123))))]
    (reduce (fn [graph node]
              (loop [neighbors (disj (set (random-sample (rand) nodes)) node)]
                (update graph node (fnil sets/union #{}) neighbors)))
            {}
            nodes)))

(deftest inverted-graph-roundtrips
  (dotimes [_ 1000]
    (let [graph (random-graph)]
      (is (= graph (-> graph
                       lattice/inverse
                       lattice/inverse
                       lattice/adjacency))))))
