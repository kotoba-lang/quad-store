# quad-store

`kotoba-lang/quad-store` is the shared CLJC home for an in-memory 4-index
Quad Arrangement (`spo`/`pso`/`pos`/`ocp`, the same naming as the deleted
kotoba-query Rust crate's EAVT/AEVT/AVET/VAET indices) plus CID-addressed
commit snapshotting via `kotoba-lang/prolly-tree`. See
`90-docs/adr/2607010930-clj-wgsl-migration.md` Phase 6.

A quad is `{:s subject :p predicate :o object}`. For this landing s/p/o are
opaque strings — dag-cbor round-trips strings exactly but not e.g. Clojure
keywords, so string-only keeps `commit!`'s snapshot honest about what
actually survives `cbor.core` encode/decode. Typed values are a follow-up.

## Use

```clojure
(require '[quad-store.core :as qs])

(def db (-> (qs/empty-db)
            (qs/assert-quad {:s "alice" :p "role" :o "admin"})
            (qs/assert-quad {:s "alice" :p "name" :o "Alice"})))

(qs/entity-attrs db "alice")          ;=> {"role" #{"admin"}, "name" #{"Alice"}}
(qs/by-predicate-value db "role" "admin") ;=> #{"alice"}

;; reverse-reference index is opt-in via a `ref?` predicate
(def db2 (qs/assert-quad db {:s "bob" :p "knows" :o "bafyalice"}
                          #(clojure.string/starts-with? % "bafy")))
(qs/refs-to db2 "bafyalice")          ;=> {"knows" #{"bob"}}

;; content-addressed commit (same db + prev -> same CID)
(def store (atom {}))
(def cid (qs/commit! (fn [c b] (swap! store assoc c b)) db nil))
```

## Scope of this landing

- 4-index hot (in-memory) Arrangement: full CRUD via `assert-quad`/
  `retract-quad`, point/scan lookups via `entity-attrs`/`by-predicate`/
  `by-predicate-value`/`refs-to`.
- `commit!` snapshots each index to a `prolly-tree` and CID-addresses the
  commit as `{index-roots prev}` — a real commit chain, content-addressed
  (verified by test: same db+prev always yields the same commit CID).

**Not in this landing** (tracked follow-ups, not silently omitted):
- Cold (post-commit, prolly-tree-backed) query — the 4 index roots are
  persisted but nothing here reconstructs a hot index from them yet.
- Typed values (only opaque strings round-trip through `commit!` today).
- Full Datalog fixpoint / SPARQL BGP evaluation — see `kotoba-lang/kqe`,
  which queries the *hot* db directly, not a committed snapshot.

## Test

```bash
clojure -M:test
```

## License

MIT
