(ns arrangement.datalog
  "Conjunctive multi-clause Datalog join over `arrangement.query`'s single
  triple-pattern router -- the Datomic-shaped `:find`/`:where` surface this
  substrate was missing (`arrangement.query`/`kotobase-peer.core/q` are both
  single-`[s p o]`-pattern only, no variable binding across clauses).

  A clause is `[e a v]` where each position is a bound value, a logic
  variable (a symbol whose name starts with `?`, e.g. `?x`), or the
  wildcard `_`. `q` binds variables left-to-right across `:where` clauses
  via nested-loop join (each clause's still-unbound variables become
  wildcards in the pattern handed to `arrangement.query/query`; already-
  bound variables are substituted in as concrete values and re-checked
  against each candidate row), and projects the `:find` variables.

  Deliberately NOT here yet (tracked as a staged roadmap, not a hidden
  gap): negation (`not` clauses), aggregation (`:with`/`count`/`sum` etc.
  in `:find`), and recursive rules (`:rules` + naive/semi-naive fixpoint).
  Each of those composes on top of this join without changing it."
  (:require [arrangement.query :as query]))

(defn- lvar?
  "True for a Datalog logic variable: a symbol whose name starts with `?`.
  `_` is the wildcard, not a variable -- it never binds."
  [x]
  (and (symbol? x) (not= x '_) (= \? (first (name x)))))

(defn- wildcard? [x] (= x '_))

(defn- substitute
  "`clause` position -> concrete `arrangement.query` pattern position: a
  wildcard or an unbound variable becomes `nil` (query's own wildcard);
  a bound variable becomes its current value; anything else passes through
  as the literal it already is."
  [term binding]
  (cond
    (wildcard? term) nil
    (lvar? term)     (get binding term)
    :else            term))

(defn- unify
  "Extend `binding` with `clause`'s variables against one matched `row`
  (`{:s :p :o}`). Returns the extended binding, or nil if a variable bound
  earlier in this same clause conflicts with `row`'s value at another
  position (e.g. `[?x :likes ?x]` against a row where s != o)."
  [binding clause row]
  (reduce (fn [b [term slot]]
            (cond
              (or (wildcard? term) (not (lvar? term))) b
              (contains? b term) (if (= (get b term) (get row slot)) b (reduced nil))
              :else (assoc b term (get row slot))))
          binding
          (map vector clause [:s :p :o])))

(defn- join-clause
  "One step of the nested-loop join: for every binding so far, resolve
  `clause` (with already-bound variables substituted in) against `db`, and
  extend that binding with each matching row's new variables."
  [bindings clause db visible?]
  (into #{}
        (mapcat (fn [binding]
                  (let [pattern (mapv #(substitute % binding) clause)]
                    (keep #(unify binding clause %)
                          (query/query db pattern visible?)))))
        bindings))

(defn q
  "`{:find [?var ...] :where [[e a v] ...]}` over `db`. `visible?` is
  required and threaded into every underlying `arrangement.query/query`
  call, same convention as `arrangement.query` itself (ADR-2607050500).
  Returns a set of `:find`-ordered vectors -- `nil` for any `:find` var a
  clause never bound (e.g. wildcard-only clauses)."
  [db {:keys [find where]} visible?]
  (let [bindings (reduce (fn [bindings clause] (join-clause bindings clause db visible?))
                         #{{}}
                         where)]
    (into #{} (map (fn [binding] (mapv #(get binding %) find))) bindings)))
