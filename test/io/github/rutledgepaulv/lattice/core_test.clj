(ns io.github.rutledgepaulv.lattice.core-test
  (:require [clojure.core.async :as async]
            [clojure.set :as sets]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [io.github.rutledgepaulv.lattice.core :as lattice]
            [io.github.rutledgepaulv.lattice.protocols :as protos]))

(defn random-graph []
  (let [nodes (random-sample (rand) (map keyword (map (comp str char) (range 97 123))))]
    (reduce
      (fn [graph node]
        (loop [neighbors (disj (set (random-sample (rand) nodes)) node)]
          (update graph node (fnil sets/union #{}) neighbors)))
      {}
      nodes)))

(defn generate-missing-test-stubs [source-ns target-ns]
  (let [missing-decls
        (->> (ns-publics (the-ns source-ns))
             (map first)
             (map (fn [name] (symbol (str name "-test"))))
             (remove (set (keys (ns-publics target-ns))))
             (sort)
             (map (fn [name] (pr-str (list 'deftest name))))
             (cons "\n\n")
             (str/join "\n\n"))
        target-file
        (str "test/" (str/replace (str/replace (str target-ns) "." "/") "-" "_") ".clj")]
    (spit target-file missing-decls :append true)
    (require target-ns :reload)))

(comment
  (generate-missing-test-stubs
    'io.github.rutledgepaulv.lattice.core
    'io.github.rutledgepaulv.lattice.core-test)
  )

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

(deftest adjacency-test
  (testing "adjacency if i only implement successors"
    (let [g        (reify
                     protos/ComputeNodes
                     (nodes [this] #{:a :b :c :d})
                     protos/ComputeSuccessors
                     (successors [this node]
                       (case node
                         :a #{:b :c :d}
                         :b #{:c}
                         :c #{:d}
                         :d #{})))
          expected {:a #{:b :c :d}
                    :b #{:c}
                    :c #{:d}
                    :d #{}}
          result   (lattice/adjacency g)]
      (is (= expected result))))
  (testing "adjacency if i only implement predecessors"
    (let [g        (reify
                     protos/ComputeNodes
                     (nodes [this] #{:a :b :c :d})
                     protos/ComputePredecessors
                     (predecessors [this node]
                       (case node
                         :a #{}
                         :b #{:a}
                         :c #{:a :b}
                         :d #{:a :c})))
          expected {:a #{:b :c :d}
                    :b #{:c}
                    :c #{:d}
                    :d #{}}
          result   (lattice/adjacency g)]
      (is (= expected result)))))

(deftest ancestors-breadth-first-test
  )

(deftest ancestors-depth-first-test)

(deftest ancestors-subgraph-test)

(deftest bidirectional-test)

(deftest branches-test)

(deftest children-test
  (testing "when there are children"
    (let [g {:a [:b :c :d]}]
      (is (= #{:b :c :d} (lattice/children g :a)))))
  (testing "when there are no children"
    (let [g {:a [:b :c :d]}]
      (is (= #{} (lattice/children g :c))))))

(deftest complement-test
  (testing "complement of a graph with no edges"
    (let [g        {:a #{} :b #{} :c #{} :d #{}}
          expected {:a #{:b :c :d} :b #{:a :c :d} :c #{:a :b :d} :d #{:a :b :c}}
          result   (lattice/complement g)]
      (is (lattice/eq expected result))))
  (testing "complement of a graph with some edges"
    (let [g        {:a [:b :c] :b [:c] :c [:d] :d [:a]}
          expected {:a #{:d} :b #{:a :d} :c #{:a :b} :d #{:b :c}}
          result   (lattice/complement g)]
      (is (lattice/eq expected result))))
  (testing "complement of a graph with all edges"
    (let [g        {:a [:b :c] :b [:c] :c [:d] :d [:a]}
          expected {:a #{} :b #{} :c #{} :d #{}}
          result   (lattice/complement (lattice/complete g))]
      (is (lattice/eq expected result)))))

(deftest complete-test)

(deftest component-subgraph-test)

(deftest components-test)

(deftest consumers-test)

(deftest degree-test)

(deftest descendants-breadth-first-test)

(deftest descendants-depth-first-test)

(deftest descendants-subgraph-test)

(deftest difference-test)

(deftest edges-test)

(deftest empty-test)

(deftest filter-nodes-test)

(deftest inbound-edges-test)

(deftest interior-test)

(deftest intersection-test
  (testing "identical graphs have an identical intersection"
    (let [g1     {:a [:b :c :d] :d [:b]}
          g2     {:a [:b :c :d] :d [:b]}
          result (lattice/intersection g1 g2)]
      (is (lattice/eq result g1 g2))))
  (testing "disparate graphs have an empty intersection"
    (let [g1     {:a [:b :c :d] :d [:b]}
          g2     {:e [:f :g :h] :h [:f]}
          result (lattice/intersection g1 g2)]
      (is (lattice/eq {} result))))
  (testing "shared nodes are included in the intersection even if there are no edges"
    (let [g1     {:a []}
          g2     {:a []}
          result (lattice/intersection g1 g2)]
      (is (lattice/eq g1 result))))
  (testing "partially overlapping graphs produce an intersection with only shared edges and nodes"
    (let [g1       {:a [:b :c] :d [:c] :e [:a]}
          g2       {:a [:c :d] :d [:c]}
          expected {:c #{}, :d #{:c}, :a #{:c}}
          result   (lattice/intersection g1 g2)]
      (is (lattice/eq expected result)))))

(deftest inverse-test
  (testing "the inverse of the empty graph is an empty graph"
    (is (lattice/eq {} (lattice/inverse {}))))
  (testing "the inverse of a graph with no edges is itself"
    (let [g {:b #{}, :a #{}}]
      (is (lattice/eq g (lattice/inverse g)))))
  (testing "the union of a graph and its inverse is the bidirectional graph"
    (dotimes [_ 100]
      (let [g (random-graph)]
        (is (lattice/eq
              (lattice/bidirectional g)
              (lattice/union g (lattice/inverse g))))))))

(deftest leaves-test)

(deftest map-nodes-test)

(deftest mapcat-nodes-test)

(deftest neighbors-test)

(deftest nodes-test)

(deftest optimize-test)

(deftest outbound-edges-test)

(deftest parent-test)

(deftest predecessors-test)

(deftest producers-test)

(deftest reduce-async-test)

(deftest reduce-blocking-test)

(deftest remove-nodes-test)

(deftest root-test)

(deftest shortest-paths-test)

(deftest sink-test)

(deftest sinks-test)

(deftest source-test)

(deftest sources-test)

(deftest subgraph?-test)

(deftest successors-test)

(deftest supergraph?-test)

(deftest topological-sort-test)

(deftest transitive-closure-test)

(deftest transitive-preserving-filter-nodes-test)

(deftest transitive-preserving-remove-nodes-test)

(deftest union-test
  (testing "identical graphs produce an identical union"
    (let [g1     {:a [:b :c :d] :d [:b]}
          g2     {:a [:b :c :d] :d [:b]}
          result (lattice/union g1 g2)]
      (is (lattice/eq result g1 g2))))
  (testing "unions add nodes even if there are no shared edges"
    (let [g1       {:a [:b :c :d] :d [:b]}
          g2       {:e [:f :g :h] :h [:f]}
          expected {:e #{:g :h :f},
                    :h #{:f},
                    :d #{:b},
                    :a #{:c :b :d}}
          result   (lattice/union g1 g2)]
      (is (lattice/eq expected result))))
  (testing "unions add edges even between existing nodes"
    (let [g1       {:a [:b :c :d] :d [:b]}
          g2       {:b [:c]}
          expected {:a [:b :c :d]
                    :b [:c]
                    :d [:b]}
          result   (lattice/union g1 g2)]
      (is (lattice/eq expected result))))
  (testing "unions can add new nodes and edges between existing nodes"
    (let [g1       {:a [:b :c :d] :d [:b]}
          g2       {:b [:c]}
          expected {:a [:b :c :d]
                    :b [:c]
                    :d [:b]}
          result   (lattice/union g1 g2)]
      (is (lattice/eq expected result)))))

(deftest with-edge-test
  (is (-> (lattice/empty)
          (lattice/with-edge :a :b)
          (lattice/eq {:a #{:b}, :b #{}}))))

(deftest with-node-test
  (is (-> (lattice/empty)
          (lattice/with-node :a)
          (lattice/eq {:a #{}}))))

(deftest without-edge-test
  (is (-> (lattice/empty)
          (lattice/with-edge :a :b)
          (lattice/without-edge :a :b)
          (lattice/eq {:a #{} :b #{}}))))

(deftest without-node-test
  (is (-> (lattice/empty)
          (lattice/with-edge :a :b)
          (lattice/without-node :a)
          (lattice/eq {:b #{}}))))

(deftest eq-test
  (is (lattice/eq {:a [:b :c]}
                  {:a #{:b :c}}
                  (-> (lattice/empty)
                      (lattice/with-edge :a :b)
                      (lattice/with-edge :a :c)))))

(deftest exterior-test
  )

(deftest sorted-test
  (testing "sorted graph"
    (let [g        {:a [:d :b :c] :d [:b] :b [:c] :c []}
          expected "{:a #{:b :c :d}, :b #{:c}, :c #{}, :d #{:b}}"
          sorted   (lattice/sorted g compare)]
      (is (= expected (pr-str (lattice/adjacency sorted)))))))