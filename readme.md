## Lattice

An elegant library for working with directed graphs in Clojure.

---

## Usage

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