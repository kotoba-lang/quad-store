(ns arrangement.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [arrangement.core :as qs]
            [multiformats.core :as mf]
            [prolly-tree.core :as pt]
            [ipld.core :as ipld])
  #?(:clj (:import [javax.crypto Cipher Mac]
                   [javax.crypto.spec SecretKeySpec GCMParameterSpec]
                   [java.util Base64])))

(defn- mem-store []
  (let [store (atom {})]
    {:put! (fn [cid bytes] (swap! store assoc cid bytes))
     :get-fn (fn [cid] (get @store cid))
     :store store}))

;; ── ADR-2607051000 test crypto (accepted 2026-07-06) ────────────────────────
;; Real AES-256-GCM + HMAC-SHA256, not a mock -- JVM (javax.crypto) only for
;; now. Worker/`crypto.subtle` support is an explicit, separately-tracked
;; follow-up (ADR-2607061900's sequencing decision), so every test that calls
;; `qs/commit!` (which now REQUIRES `blind-fn`/`encrypt-fn`, no silent
;; default) is `:clj`-only below; the `:cljs` node-test build simply doesn't
;; see them yet, same as any other not-yet-ported-to-cljs primitive in this
;; codebase (`kotoba-lang/signal`'s own README precedent).
#?(:clj
   (do
     (def ^:private test-dek
       (SecretKeySpec. (byte-array (range 1 33)) "AES"))
     (def ^:private test-blind-key
       (SecretKeySpec. (byte-array (range 33 65)) "HmacSHA256"))
     (def ^:private test-nonce-key
       (SecretKeySpec. (byte-array (range 65 97)) "HmacSHA256"))

     (defn test-encrypt-fn
       "AES-256-GCM seal: 12-byte nonce ++ ciphertext-with-tag, one combined
       byte blob (same wire shape kotoba-crypto's own hpke.rs uses: nonce ++
       ciphertext, no separate framing needed).

       The nonce is DERIVED, not random: HMAC-SHA256(test-nonce-key,
       plaintext) truncated to 12 bytes -- a simplified synthetic-IV /
       deterministic-AEAD construction (a known-good composition of two
       standard primitives, not a hand-rolled cipher). This matters for
       `arrangement.core/commit!`'s content-addressing: a RANDOM-nonce
       encrypt-fn would make even a byte-identical db produce a different
       snapshot CID on every commit, silently breaking `fold!`'s documented
       'concurrent folds of the same state are... cheap [no-op-restore]'
       property. Nonce-uniqueness-per-plaintext (what GCM actually requires)
       still holds: two DIFFERENT plaintexts under the same key get
       different HMAC outputs, hence different nonces, with overwhelming
       probability -- only *identical* plaintexts intentionally reuse the
       same nonce, which is safe (same ciphertext both times, not a
       different-plaintext collision)."
       [^bytes plaintext]
       (let [mac (Mac/getInstance "HmacSHA256")
             _ (.init mac test-nonce-key)
             nonce (byte-array (take 12 (.doFinal mac plaintext)))
             cipher (Cipher/getInstance "AES/GCM/NoPadding")]
         (.init cipher Cipher/ENCRYPT_MODE test-dek (GCMParameterSpec. 128 nonce))
         (byte-array (concat nonce (.doFinal cipher plaintext)))))

     (defn test-decrypt-fn
       "Inverse of `test-encrypt-fn`: split the leading 12-byte nonce back off,
       decrypt the rest."
       [^bytes blob]
       (let [nonce (byte-array (take 12 blob))
             ct (byte-array (drop 12 blob))
             cipher (Cipher/getInstance "AES/GCM/NoPadding")]
         (.init cipher Cipher/DECRYPT_MODE test-dek (GCMParameterSpec. 128 nonce))
         (.doFinal cipher ct)))

     (defn test-blind-fn
       "Keyed, deterministic MAC (HMAC-SHA256 -> base64) over the printed
       component -- same value in, same blinded token out, so a caller who
       knows the plaintext prefix can independently re-derive it to seek."
       [component]
       (let [mac (Mac/getInstance "HmacSHA256")]
         (.init mac test-blind-key)
         (let [digest (.doFinal mac (.getBytes (pr-str component) "UTF-8"))]
           (.encodeToString (Base64/getEncoder) digest))))))

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

;; ── ADR-2607051000 (accepted 2026-07-06): ciphertext-over-CID persistence ───
;; `qs/commit!`/`index-root` now REQUIRE `blind-fn`/`encrypt-fn` -- no silent
;; default, matching this codebase's established `schema-version`/`visible?`
;; discipline (ADR-2607050700). `:clj`-only below (see the crypto block
;; above); the `:cljs` node-test build doesn't compile these until the
;; Worker/`crypto.subtle` follow-up lands.
#?(:clj
   (do

     (deftest test-crypto-helpers-are-deterministic-and-round-trip
       (testing "encrypt-fn is deterministic per plaintext (required for commit! idempotency, see commit-is-content-addressed)"
         (is (= (seq (test-encrypt-fn (.getBytes "same input" "UTF-8")))
                (seq (test-encrypt-fn (.getBytes "same input" "UTF-8"))))))
       (testing "encrypt-fn output differs for different plaintexts (nonce doesn't collide in practice)"
         (is (not= (seq (test-encrypt-fn (.getBytes "input a" "UTF-8")))
                   (seq (test-encrypt-fn (.getBytes "input b" "UTF-8"))))))
       (testing "decrypt-fn inverts encrypt-fn"
         (is (= "round-trip me" (String. ^bytes (test-decrypt-fn (test-encrypt-fn (.getBytes "round-trip me" "UTF-8"))) "UTF-8"))))
       (testing "blind-fn is deterministic and one-way (doesn't leak the plaintext in the token)"
         (is (= (test-blind-fn "alice") (test-blind-fn "alice")))
         (is (not (str/includes? (test-blind-fn "alice") "alice")))))

     (deftest commit-preserves-link-values-through-index-root
       (let [{:keys [put! get-fn]} (mem-store)
             db (qs/assert-quad (qs/empty-db) {:s "alice" :p "knows" :o bafy-link})
             cid (qs/commit! put! db nil qs/current-schema-version
                              test-blind-fn test-encrypt-fn)
             node (ipld/decode (get-fn cid))
             spo-root (ipld/link-cid (get-in node ["index-roots" "spo"]))
             [[_ leaf-val]] (pt/scan-prefix get-fn spo-root "")
             [_ _ v] (mapv qs/edn->link (ipld/decode (test-decrypt-fn leaf-val)))]
         (is (= bafy-link v)
             "the Link comes back out of the persisted, encrypted index VALUE intact -- not the key, which is one-way blinded and cannot be inverted")))

     (deftest index-root-key-is-blinded-value-is-encrypted-not-plaintext
       ;; The correctness bug this test guards against: ADR-2607051000's
       ;; original text claimed "no separate value encryption step is needed
       ;; -- the datom's actual payload... is exactly the s/p/o triple that's
       ;; now blinded." That's wrong (HMAC is one-way); the addendum
       ;; (ADR-2607061800/2607061900 follow-up) corrected it: the VALUE now
       ;; carries the encrypted triple, the KEY stays blind-only. This test
       ;; asserts both halves of that correction directly against real
       ;; persisted bytes, not just the round-trip.
       (let [{:keys [put! get-fn]} (mem-store)
             secret "admin-secret-value"
             db (qs/assert-quad (qs/empty-db) {:s "alice" :p "role" :o secret})
             cid (qs/commit! put! db nil qs/current-schema-version
                              test-blind-fn test-encrypt-fn)
             node (ipld/decode (get-fn cid))
             spo-root (ipld/link-cid (get-in node ["index-roots" "spo"]))
             [[leaf-key leaf-val]] (pt/scan-prefix get-fn spo-root "")]
         (testing "the leaf key never contains any plaintext component"
           (is (not (str/includes? leaf-key "alice")))
           (is (not (str/includes? leaf-key "role")))
           (is (not (str/includes? leaf-key secret))))
         (testing "the leaf value is opaque ciphertext bytes, not the plaintext triple"
           (is (bytes? leaf-val))
           (is (not (str/includes? (String. ^bytes leaf-val "ISO-8859-1") secret))))
         (testing "decrypting the value recovers the real triple"
           (is (= ["alice" "role" secret]
                  (ipld/decode (test-decrypt-fn leaf-val)))))))

     (deftest commit-is-content-addressed
       (let [{:keys [put! store]} (mem-store)
             db (-> (qs/empty-db)
                    (qs/assert-quad {:s "alice" :p "role" :o "admin"})
                    (qs/assert-quad {:s "bob" :p "role" :o "user"}))
             cid1 (qs/commit! put! db nil qs/current-schema-version
                               test-blind-fn test-encrypt-fn)
             cid2 (qs/commit! put! db nil qs/current-schema-version
                               test-blind-fn test-encrypt-fn)]
         (testing "same db + prev -> same commit CID"
           ;; Preserved as `=` (not `not=`) specifically because
           ;; `test-encrypt-fn` derives its nonce deterministically from the
           ;; plaintext -- content-addressing idempotency (which `fold!`'s
           ;; own docstring relies on: "concurrent folds of the same state
           ;; are safe, redundant, and cheap") only survives encryption if
           ;; the encrypt-fn a caller supplies is itself deterministic per
           ;; plaintext. A random-nonce encrypt-fn would make this `not=`
           ;; instead -- a real, silent regression this test would have
           ;; caught if `test-encrypt-fn` had stayed random-nonce.
           (is (= cid1 cid2)))
         (testing "different prev -> different commit CID"
           (is (not= cid1 (qs/commit! put! db (mf/kotoba-cid "some-other-prev") qs/current-schema-version
                                       test-blind-fn test-encrypt-fn))))
         (testing "different db -> different commit CID"
           (let [db2 (qs/assert-quad db {:s "carol" :p "role" :o "user"})]
             (is (not= cid1 (qs/commit! put! db2 nil qs/current-schema-version
                                         test-blind-fn test-encrypt-fn)))))
         (testing "different schema-version -> different commit CID"
           (is (not= cid1 (qs/commit! put! db nil 2 test-blind-fn test-encrypt-fn))))
         (is (contains? @store cid1))))

     (deftest commit-block-is-real-ipld
       (let [{:keys [put! get-fn]} (mem-store)
             db (-> (qs/empty-db)
                    (qs/assert-quad {:s "alice" :p "role" :o "admin"}))
             prev (qs/commit! put! (qs/empty-db) nil qs/current-schema-version
                               test-blind-fn test-encrypt-fn)
             cid (qs/commit! put! db prev qs/current-schema-version
                              test-blind-fn test-encrypt-fn)
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

     (deftest commit-schema-version-is-required-and-caller-declared
       ;; ADR-2607050500 "Schema evolution": the caller states the version being
       ;; written -- no silent default, and a different version really does
       ;; produce a different persisted node.
       (let [{:keys [put! get-fn]} (mem-store)
             cid (qs/commit! put! (qs/empty-db) nil qs/current-schema-version
                              test-blind-fn test-encrypt-fn)
             node (ipld/decode (get-fn cid))]
         (is (= qs/current-schema-version (get node "schema-version")))
         (is (= 2 (get (ipld/decode (get-fn (qs/commit! put! (qs/empty-db) nil 2
                                                          test-blind-fn test-encrypt-fn)))
                       "schema-version")))))))
