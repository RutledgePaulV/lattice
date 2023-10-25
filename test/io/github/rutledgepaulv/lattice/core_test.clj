(ns io.github.rutledgepaulv.lattice.core-test
  (:require [clojure.core.async :as async]
            [clojure.set :as sets]
            [clojure.test :refer :all]
            [io.github.rutledgepaulv.lattice.core :as lattice]))

(defn random-graph []
  (let [nodes (random-sample (rand) (map keyword (map (comp str char) (range 97 123))))]
    (reduce
      (fn [graph node]
        (loop [neighbors (disj (set (random-sample (rand) nodes)) node)]
          (update graph node (fnil sets/union #{}) neighbors)))
      {}
      nodes)))

(deftest inverted-graph-roundtrips-test
  (dotimes [_ 1000]
    (let [graph (random-graph)]
      (is (= graph (-> graph
                       lattice/inverse
                       lattice/inverse
                       lattice/adjacency))))))

(deftest reduce-test
  (let [graph           {:a [:b :c :d] :d [:b]}
        expected1       {:errors  {}
                         :pending #{}
                         :visited #{:a :b :c :d}
                         :result  {:a {}
                                   :c {:a {}}
                                   :d {:a {}}
                                   :b {:a {} :d {:a {}}}}}
        ; it could include C too depending on which thread wins
        expected2       (update-in expected1 [:result :b] assoc :c (get-in expected1 [:result :c]))
        result-compute  (-> graph
                            (lattice/reduce
                              (fn [state node]
                                (assoc-in state [node] state)))
                            (async/<!!))
        result-async    (-> graph
                            (lattice/reduce-async
                              (fn [state node]
                                (async/go (assoc-in state [node] state))))
                            (async/<!!))
        result-blocking (-> graph
                            (lattice/reduce-blocking
                              (fn [state node]
                                (assoc-in state [node] state)))
                            (async/<!!))]
    (is (contains? #{expected1 expected2} result-compute))
    (is (contains? #{expected1 expected2} result-async))
    (is (contains? #{expected1 expected2} result-blocking))))