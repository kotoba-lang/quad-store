(ns quad-store.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [quad-store.core :as qs]))

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
      (is (not= cid1 (qs/commit! put! db "some-other-prev"))))
    (testing "different db -> different commit CID"
      (let [db2 (qs/assert-quad db {:s "carol" :p "role" :o "user"})]
        (is (not= cid1 (qs/commit! put! db2 nil)))))
    (is (contains? @store cid1))))
