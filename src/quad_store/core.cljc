(ns quad-store.core
  "In-memory 4-index Quad Arrangement -- `spo`/`pso`/`pos`/`ocp`, the same
  naming as the deleted kotoba-query Rust crate's EAVT/AEVT/AVET/VAET
  indices -- plus CID-addressed commit snapshotting via `prolly-tree.core`.

  A quad is `{:s subject :p predicate :o object}`. For this landing s/p/o
  are treated as opaque strings (typed values are a follow-up: dag-cbor
  round-trips strings exactly but not e.g. keywords, so string-only keeps
  `commit!`'s snapshot honest about what actually survives encode/decode).

  `:o` values that are themselves CIDs (references to other entities) are
  additionally indexed in `ocp` for reverse-reference lookup when the
  caller passes a `ref?` predicate to `assert-quad`/`retract-quad`; quads
  asserted without one are simply not reverse-indexed."
  (:require [prolly-tree.core :as pt]
            [ipld.core :as ipld]))

(defn empty-db [] {:spo {} :pso {} :pos {} :ocp {}})

(defn- upd [m k1 k2 v]
  (update m k1 (fnil (fn [m2] (update m2 k2 (fnil conj #{}) v)) {})))

(defn- rm [m k1 k2 v]
  (if-let [m2 (get m k1)]
    (let [s (disj (get m2 k2 #{}) v)]
      (if (empty? s)
        (let [m2' (dissoc m2 k2)]
          (if (empty? m2') (dissoc m k1) (assoc m k1 m2')))
        (assoc m k1 (assoc m2 k2 s))))
    m))

(defn assert-quad
  "Add `{:s :p :o}` to `db`'s 4 indices. `ref?` (default: never) decides
  whether `:o` is also indexed in `:ocp` for reverse-reference lookup."
  ([db q] (assert-quad db q (constantly false)))
  ([db {:keys [s p o]} ref?]
   (cond-> db
     true     (update :spo upd s p o)
     true     (update :pso upd p s o)
     true     (update :pos upd p o s)
     (ref? o) (update :ocp upd o p s))))

(defn retract-quad
  "Remove `{:s :p :o}` from `db`'s 4 indices."
  ([db q] (retract-quad db q (constantly false)))
  ([db {:keys [s p o]} ref?]
   (cond-> db
     true     (update :spo rm s p o)
     true     (update :pso rm p s o)
     true     (update :pos rm p o s)
     (ref? o) (update :ocp rm o p s))))

(defn entity-attrs
  "All `{p #{o...}}` for subject `s` (EAVT-style)."
  [db s] (get (:spo db) s {}))

(defn by-predicate
  "All `{s #{o...}}` for predicate `p` (AEVT-style scan)."
  [db p] (get (:pso db) p {}))

(defn by-predicate-value
  "All subjects `s` where `[s p o]` holds (AVET-style point lookup)."
  [db p o] (get-in db [:pos p o] #{}))

(defn refs-to
  "All `{p #{s...}}` referencing object `o` (VAET-style reverse lookup) --
  only populated for quads asserted with a truthy `ref?`."
  [db o] (get (:ocp db) o {}))

(defn- index-root
  "Flatten one index map into sorted `[key val]` prolly-tree entries and
  build a tree from it. `key` is the printed `[k1 k2 v]` triple so the
  whole index is content-addressed by its full (s,p,o)-equivalent set;
  `val` is `true` (membership-only encoding, no separate value payload)."
  [put! m]
  (let [entries (sort-by first
                         (for [[k1 m2] m [k2 vs] m2 v vs]
                           [(pr-str [k1 k2 v]) true]))]
    (pt/build-tree put! entries)))

(defn commit!
  "Snapshot `db`'s 4 indices into 4 prolly-trees via `put!`
  (`prolly-tree.core`-shaped port: `(put! cid bytes)`), CID-address the
  commit itself (dag-cbor of `{index-roots prev}`, where every root and
  `prev` is a REAL tag-42 IPLD link via `kotoba-lang/ipld` -- an empty
  index snapshots as null), and return the commit CID string. `prev` is
  the previous commit CID, or nil for the first commit. Content-addressed:
  committing the same `db` + `prev` twice returns the same CID."
  [put! db prev]
  (let [->link #(some-> % ipld/link)          ; empty index -> nil root -> null
        roots {"spo" (->link (index-root put! (:spo db)))
               "pso" (->link (index-root put! (:pso db)))
               "pos" (->link (index-root put! (:pos db)))
               "ocp" (->link (index-root put! (:ocp db)))}]
    (ipld/put-node! put! {"index-roots" roots "prev" (->link prev)})))
