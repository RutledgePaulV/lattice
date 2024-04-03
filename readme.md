## Lattice

An algebra for working with directed graphs in Clojure.

---

## Basic Usage

Lattice [implements the necessary protocols for IPersistentMap](./src/io/github/rutledgepaulv/lattice/impls/concrete.clj),
so you can start using it with adjacency maps right away.

```clojure
(require '[io.github.rutledgepaulv.lattice.core :as lattice])

(def graph {:a [1 2 3] :b [3]})

(lattice/nodes graph)
; => #{1 3 2 :b :a}

(lattice/edges graph)
; => #{[:a 2] [:a 3] [:a 1] [:b 3]}

(lattice/sources graph)
; => #{:b :a}

(lattice/sinks graph)
; => #{1 3 2}

(lattice/inverse graph)
; => {1 #{:a}, 3 #{:b :a}, 2 #{:a}, :b #{}, :a #{}}
```

## Concurrent Reductions

Graphs are frequently used to express dependencies between nodes. It's
useful to be able to process/reduce over those graphs in a way that
respects dependencies. A naive approach linearly reduces over the
topological sort, but for many graphs we can do much better than that.
Lattice provides support for fork-join reductions that leverage
parallelism between independent subgraphs.

```clojure
(require '[io.github.rutledgepaulv.lattice.core :as lattice])
(require '[clojure.core.async :as async])

; use lattice/reduce for computation tasks
; use lattice/reduce-async for async tasks
; use lattice/reduce-blocking for blocking tasks

(async/<!!
  (lattice/reduce
    {:a [:b :c :d] :d [:b]}
    (fn [state node]
      (assoc-in state [node] state))))

; =>
; you can see we fork after processing :a
; so :c and :d can process simultaneously
; then, once :d is done, we can process :b

#_{:errors  {}
   :pending #{}
   :visited #{:a :b :c :d}
   :result  {:a {}
             :c {:a {}}
             :d {:a {}}
             :b {:a {} :d {:a {}}}}}
```

## Extensions

Lattice uses Clojure protocols to define all functionality in abstract terms while allowing
for concrete type performance optimizations. Every single function is built on just two
underlying protocol functions: `(nodes graph)` and *either* `(successors graph node)`
or `(predecessors graph node)`. Default implementations are already defined in terms
of one another; it does not matter which you implement. However, if you don't implement
either it's a mutually recursive loop that'll blow your stack!

## Prior Art

- [Loom](https://github.com/aysylu/loom)
- [Ubergraph](https://github.com/Engelberg/ubergraph)
- [missing.topology](https://github.com/RutledgePaulV/missing/blob/develop/src/missing/topology.clj)