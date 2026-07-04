(ns quad-store.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [quad-store.core :as qs]
            [multiformats.core :as mf]
            [prolly-tree.core :as pt]
            [ipld.core :as ipld]))

(defn- mem-store []
  (let [store (atom {})]
    {:put! (fn [cid bytes] (swap! store assoc cid bytes))
     :get-fn (fn [cid] (get @store cid))
     :store store}))

(deftest assert-and-lookup
  (let [db (-> (qs/empty-db)
               (qs/assert-quad {:s "alice" :p "role" :o "admin"})
               (qs/assert-quad {:s "alice" :p "name" :o "Alice"})
               (qs/assert-quad {:s "bob" :p "role" :o "user"}))]
    (is (= {"role" #{"admin"} "name" #{"Alice"}} (qs/entity-attrs db "alice")))
    (is (= {"alice" #{"admin"} "bob" #{"user"}} (qs/by-predicate db "role")))
    (is (= #{"alice"} (qs/by-predicate-value db "role" "admin")))
    (is (= #{"bob"} (qs/by-predicate-value db "role" "user")))))

(deftest retract-removes-from-all-4-indices
  (let [db (-> (qs/empty-db)
               (qs/assert-quad {:s "alice" :p "role" :o "admin"})
               (qs/retract-quad {:s "alice" :p "role" :o "admin"}))]
    (is (= {} (qs/entity-attrs db "alice")))
    (is (= {} (qs/by-predicate db "role")))
    (is (= #{} (qs/by-predicate-value db "role" "admin")))))

(deftest ref-indexing-is-opt-in
  (let [ref? #(str/starts-with? % "bafy")
        db (-> (qs/empty-db)
               (qs/assert-quad {:s "alice" :p "knows" :o "bafybob"} ref?)
               (qs/assert-quad {:s "alice" :p "name" :o "Alice"} ref?))]
    (is (= {"knows" #{"alice"}} (qs/refs-to db "bafybob")))
    (is (= {} (qs/refs-to db "Alice")) "non-ref object is not reverse-indexed")))

(def ^:private bafy-link
  (ipld/link "bafyreiaakutsdtndrl7e7emcmkp5hjsaaq2vu6prfelbgaglprvtdon63m"))

(deftest ref-indexing-naturalizes-to-ipld-link
  ;; ADR-2607050200: ref? defaults to ipld/link? instead of requiring every
  ;; caller to pass its own predicate (ADR-2607023200 §6-4).
  (let [db (-> (qs/empty-db)
               (qs/assert-quad {:s "alice" :p "knows" :o bafy-link})
               (qs/assert-quad {:s "alice" :p "name" :o "Alice"}))]
    (testing "default ref? indexes Link values automatically"
      (is (= {"knows" #{"alice"}} (qs/refs-to db bafy-link))))
    (testing "a plain string is never mistaken for a ref, even one shaped like a CID"
      (is (= {} (qs/refs-to db "Alice"))))
    (testing "retract-quad's matching default un-indexes it"
      (let [db2 (qs/retract-quad db {:s "alice" :p "knows" :o bafy-link})]
        (is (= {} (qs/refs-to db2 bafy-link)))))))

(deftest link-edn-safe-roundtrip
  (testing "a Link survives pr-str/edn-read-string via the edn-safe form"
    (is (= bafy-link (qs/edn->link (edn/read-string (pr-str (qs/link->edn bafy-link)))))))
  (testing "non-Link values pass through both directions unchanged"
    (is (= "alice" (qs/link->edn "alice")))
    (is (= "alice" (qs/edn->link "alice")))))

(deftest commit-preserves-link-values-through-index-root
  (let [{:keys [put! get-fn]} (mem-store)
        db (qs/assert-quad (qs/empty-db) {:s "alice" :p "knows" :o bafy-link})
        cid (qs/commit! put! db nil)
        node (ipld/decode (get-fn cid))
        spo-root (ipld/link-cid (get-in node ["index-roots" "spo"]))
        [[leaf-key _]] (pt/scan-prefix get-fn spo-root "")
        [_ _ v] (mapv qs/edn->link (edn/read-string leaf-key))]
    (is (= bafy-link v) "the Link comes back out of the persisted index intact")))

(deftest commit-is-content-addressed
  (let [{:keys [put! store]} (mem-store)
        db (-> (qs/empty-db)
               (qs/assert-quad {:s "alice" :p "role" :o "admin"})
               (qs/assert-quad {:s "bob" :p "role" :o "user"}))
        cid1 (qs/commit! put! db nil)
        cid2 (qs/commit! put! db nil)]
    (testing "same db + prev -> same commit CID"
      (is (= cid1 cid2)))
    (testing "different prev -> different commit CID"
      (is (not= cid1 (qs/commit! put! db (mf/kotoba-cid "some-other-prev")))))
    (testing "different db -> different commit CID"
      (let [db2 (qs/assert-quad db {:s "carol" :p "role" :o "user"})]
        (is (not= cid1 (qs/commit! put! db2 nil)))))
    (is (contains? @store cid1))))

(deftest commit-block-is-real-ipld
  (let [{:keys [put! get-fn]} (mem-store)
        db (-> (qs/empty-db)
               (qs/assert-quad {:s "alice" :p "role" :o "admin"}))
        prev (qs/commit! put! (qs/empty-db) nil)
        cid (qs/commit! put! db prev)
        node (ipld/decode (get-fn cid))]
    (testing "index roots and prev are tag-42 links (nil for empty indexes)"
      (is (ipld/link? (get-in node ["index-roots" "spo"])))
      (is (ipld/link? (get node "prev")))
      (is (= prev (ipld/link-cid (get node "prev")))))
    (testing "generic ipld/links walk reaches every root + prev, all fetchable"
      (is (seq (ipld/links node)))
      (doseq [c (ipld/links node)]
        (is (= c (ipld/cid (get-fn c))))))
    (testing "empty db commit has null roots"
      (let [n0 (ipld/decode (get-fn prev))]
        (is (nil? (get-in n0 ["index-roots" "spo"])))
        (is (nil? (get n0 "prev")))))))

(deftest commit-node-carries-schema-version
  ;; ADR-2607050500 "Schema evolution": a marker for future incompatible
  ;; index-shape changes to key a migration off of. Purely additive.
  (let [{:keys [put! get-fn]} (mem-store)
        cid (qs/commit! put! (qs/empty-db) nil)
        node (ipld/decode (get-fn cid))]
    (is (= qs/current-schema-version (get node "schema-version")))))
