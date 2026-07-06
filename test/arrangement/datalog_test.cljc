(ns arrangement.datalog-test
  (:require [clojure.test :refer [deftest is testing]]
            [arrangement.core :as arr]
            [arrangement.datalog :as dl]))

(defn- fixture-db []
  (-> (arr/empty-db)
      (arr/assert-quad {:s "alice" :p "role" :o "admin"})
      (arr/assert-quad {:s "alice" :p "name" :o "Alice"})
      (arr/assert-quad {:s "bob" :p "role" :o "user"})
      (arr/assert-quad {:s "bob" :p "name" :o "Bob"})
      (arr/assert-quad {:s "carol" :p "role" :o "admin"})
      (arr/assert-quad {:s "carol" :p "name" :o "Carol"})))

(def ^:private everything (constantly true))

(deftest single-clause-behaves-like-arrangement-query
  (let [db (fixture-db)]
    (is (= #{["alice"] ["carol"]}
           (dl/q db {:find '[?s] :where '[[?s "role" "admin"]]} everything)))))

(deftest two-clause-join-on-shared-variable
  (let [db (fixture-db)]
    (testing "?s shared between clauses restricts to admins, then projects their name"
      (is (= #{["Alice"] ["Carol"]}
             (dl/q db {:find '[?name]
                       :where '[[?s "role" "admin"]
                                [?s "name" ?name]]}
                   everything))))))

(deftest three-clause-chain-join
  (let [db (fixture-db)]
    (is (= #{["alice" "admin" "Alice"] ["carol" "admin" "Carol"]}
           (dl/q db {:find '[?s ?role ?name]
                     :where '[[?s "role" ?role]
                              [?s "role" "admin"]
                              [?s "name" ?name]]}
                 everything)))))

(deftest no-shared-variable-is-a-cartesian-product
  (let [db (fixture-db)]
    (is (= #{["alice" "Bob"] ["alice" "Carol"] ["alice" "Alice"]
             ["bob" "Bob"] ["bob" "Carol"] ["bob" "Alice"]
             ["carol" "Bob"] ["carol" "Carol"] ["carol" "Alice"]}
           (dl/q db {:find '[?s ?name]
                     :where '[[?s "role" _]
                              [_ "name" ?name]]}
                 everything)))))

(deftest conflicting-binding-yields-empty-result
  (let [db (fixture-db)]
    (testing "?x can't be both alice's own subject and match a name it never has"
      (is (= #{}
             (dl/q db {:find '[?x]
                       :where '[[?x "role" "admin"]
                                [?x "name" "Bob"]]}
                   everything))))))

(deftest repeated-variable-within-one-clause-self-joins
  (let [db (-> (arr/empty-db)
               (arr/assert-quad {:s "alice" :p "knows" :o "alice"})
               (arr/assert-quad {:s "bob" :p "knows" :o "carol"}))]
    (is (= #{["alice"]}
           (dl/q db {:find '[?x] :where '[[?x "knows" ?x]]} everything)))))

(deftest visible-is-required
  (is (thrown? #?(:clj clojure.lang.ArityException :cljs js/Error)
               (dl/q (fixture-db) {:find '[?s] :where '[[?s "role" "admin"]]}))))

(deftest visible-applies-per-clause
  (let [db (fixture-db)
        no-bob (fn [{:keys [s]}] (not= "bob" s))]
    (is (= #{["Alice"] ["Carol"]}
           (dl/q db {:find '[?name]
                     :where '[[?s "role" _]
                              [?s "name" ?name]]}
                 no-bob)))))
