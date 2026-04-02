# Functional Programming Study Spec

Lisp-first. Primary sources. From λ-calculus to modern typed FP.


## Scope

A self-directed curriculum moving from mathematical foundations through the Lisp tradition into contemporary FP research. Not a survey — a vertical reading of the canon, with exercises in Scheme and Clojure.


## Structure

Three tiers, each building on the last. Within each tier: read the paper, implement the idea, write a short reflection connecting it to the whole.

### Tier 1 — Foundations

The lambda calculus, recursion theory, and the birth of Lisp.

| # | Source | Key Concept | Exercise Language |
|---|--------|-------------|-------------------|
| 1 | Church, "An Unsolvable Problem of Elementary Number Theory" (1936) | λ-calculus, encoding, undecidability | Scheme (raw λ encodings: Church numerals, booleans, pairs) |
| 2 | McCarthy, "Recursive Functions of Symbolic Expressions" (1960) | S-expressions, `eval`/`apply`, conditional expressions | Scheme (implement a metacircular evaluator) |
| 3 | Landin, "The Next 700 Programming Languages" (1966) | ISWIM, off-side rule, the notion of denotation | Reading + written reflection |
| 4 | Backus, "Can Programming Be Liberated from the von Neumann Style?" (1978) | FP systems, algebra of programs, point-free combinators | Scheme (build a small FP-style combinator library) |

### Tier 2 — The Lisp Tradition & Core Abstractions

Lexical scope, continuations, macros, lazy evaluation, and the algebra of datatypes.

| # | Source | Key Concept | Exercise Language |
|---|--------|-------------|-------------------|
| 5 | Sussman & Steele, "Lambda: The Ultimate Imperative/Declarative" (1976–77) | Lexical scoping, tail calls, λ as universal substrate | Scheme (rewrite imperative patterns as λ forms) |
| 6 | Abelson & Sussman, *Structure and Interpretation of Computer Programs* | Abstraction barriers, streams, metalinguistic abstraction | Scheme (all exercises, minimum chapters 1–4) |
| 7 | Steele, "Lambda: The Ultimate GOTO" (1977) | CPS, continuations as first-class | Scheme (CPS-transform a tree walker) |
| 8 | Kohlbecker et al., "Hygienic Macro Expansion" (1986) | Macro systems, hygiene, syntactic abstraction | Scheme (implement `syntax-rules` subset) |
| 9 | Henderson, *Functional Programming: Application and Implementation* (1980) | Lazy evaluation, graph reduction | Reading + compare with Clojure lazy seqs |
| 10 | Wadler, "Monads for Functional Programming" (1995) | Monads as computational pattern, sequencing pure effects | Scheme (implement maybe, state, list monads manually) |

### Tier 3 — Modern FP & Applied Research

Types, effects, category theory in practice, and FP at scale.

| # | Source | Key Concept | Exercise Language |
|---|--------|-------------|-------------------|
| 11 | Hughes, "Why Functional Programming Matters" (1989) | Modularity via HOFs and lazy evaluation | Clojure (implement the numerical algorithms from the paper) |
| 12 | Wadler, "Theorems for Free!" (1989) | Parametricity, relational reasoning | Reading + derive theorems for Clojure transducer signatures |
| 13 | Okasaki, *Purely Functional Data Structures* (1998) | Amortized persistence, banker's/physicist's methods | Clojure (implement 3+ structures: queue, heap, red-black tree) |
| 14 | Peyton Jones, "Tackling the Awkward Squad" (2001) | IO, concurrency, exceptions, FFI in a pure language | Reading + compare Haskell IO with Clojure's managed references |
| 15 | Swierstra, "Data Types à la Carte" (2008) | Composable interpreters, free monads, expression problem | Clojure (build a composable DSL interpreter using protocols) |
| 16 | Kiselyov et al., "Extensible Effects" (2013) | Effect handlers, delimited continuations, alternative to monad transformers | Scheme (implement a minimal extensible effects library) |
| 17 | Hickey, "A History of Clojure" (2020, HOPL IV) | Persistent data structures, identity/state separation, practical FP | Clojure (implement a simplified persistent vector from scratch) |


## Parallel Tracks

Run alongside the main tiers, not sequentially.

### Category Theory (minimal, applied)

Read selectively — enough to ground the vocabulary, not to become a mathematician.

- Awodey, *Category Theory* — chapters 1–5 (categories, functors, natural transformations, universals, adjoints)
- Milewski, *Category Theory for Programmers* — bridge to code; pair each chapter with a Clojure/Scheme implementation

### Formal Verification & Proof Assistants (exposure)

- Pierce, *Software Foundations* vol. 1 (Coq) — at minimum the first 6 chapters
- Purpose: see the logical end of "types as propositions," even if daily work stays in Clojure

### History & Philosophy of Computation

- Turing, "On Computable Numbers" (1936) — pair with Church for the Church-Turing thesis
- Dijkstra, EWD manuscripts (selected: 340, 447, 1036) — discipline, correctness, humility before the machine
- Gabriel, "Worse Is Better" (1989) — the MIT/New Jersey tension; directly relevant to Lisp's trajectory


## Method

### Per-Source Protocol

1. **First reading** — uninterrupted, marginal annotations only
2. **Implementation** — in the specified language, no libraries unless the paper assumes them
3. **Reflection** — ≤300 words connecting the idea to (a) the prior source and (b) your daily Clojure practice
4. **Spaced review** — revisit reflections at 7d, 30d, 90d intervals

### Tooling

- **Scheme**: Chez Scheme or MIT/GNU Scheme (R7RS where possible)
- **Clojure**: standard REPL + CIDER
- **Notes**: Org-mode, one file per source, interlinked
- **Papers**: local archive, managed via Zotero or plain filesystem

### Cadence

- 1 source per week minimum for tiers 1–2
- 1 source per 2 weeks for tier 3 (heavier implementation load)
- Category theory track: 1 chapter/week in parallel
- Estimated total: 9–12 months at steady pace


## Completion Criteria

Not "finished reading" but demonstrated ability:

1. Implement a metacircular Lisp evaluator from scratch (covers sources 1–2, 6)
2. Build a non-trivial program using only pure FP constructs — no mutation, no side effects except at the boundary (covers 4, 11, 13)
3. Explain monads, continuations, and effect systems to a competent programmer without using analogies — only precise definitions (covers 7, 10, 15, 16)
4. Write a technical essay (~3000 words) tracing the line from Church through McCarthy through Hickey, arguing why the Lisp tradition matters for contemporary software (covers the entire curriculum)


## Capstone Project — *Λόγος*: A Growing Lisp

A single project that evolves across all three tiers. Each concept from the reading list is demonstrated by extending a Lisp interpreter and its standard library, with property-based and example-based tests proving each addition correct.

The project lives in one repository with two modules: `logos-scheme` (Chez Scheme, tiers 1–2) and `logos-clj` (Clojure, tier 3). The Clojure module doesn't rewrite the Scheme one — it ports the mature interpreter and builds the applied layer on top.


### Architecture

```
logos/
├── scheme/
│   ├── src/
│   │   ├── core.ss          ;; eval/apply, environment, primitives
│   │   ├── lambda.ss         ;; λ encodings, closures, lexical scope
│   │   ├── cps.ss            ;; CPS transformer
│   │   ├── macros.ss         ;; hygienic macro expander
│   │   ├── lazy.ss           ;; lazy evaluation / graph reduction
│   │   ├── monads.ss         ;; monad protocol + instances
│   │   └── effects.ss        ;; extensible effects / delimited continuations
│   └── tests/
│       └── (mirrors src/)
├── clj/
│   ├── src/logos/
│   │   ├── core.clj          ;; ported eval/apply
│   │   ├── persistent.clj    ;; purely functional data structures
│   │   ├── combinators.clj   ;; point-free combinator library
│   │   ├── dsl.clj           ;; composable DSL interpreter (à la carte)
│   │   └── pvector.clj       ;; simplified persistent vector
│   └── test/logos/
│       └── (mirrors src/)
├── docs/
│   ├── reflections/          ;; per-source org-mode reflections
│   └── adr/                  ;; architectural decision records
└── README.md
```


### Phase Map

Each phase corresponds to a tier. The deliverable column is what gets committed — code + tests + reflection.

#### Phase 1 — The Evaluator (Tier 1, sources 1–4)

| Source | Feature Added to Λόγος | Tests |
|--------|----------------------|-------|
| Church (1) | Church numeral encoding: arithmetic via pure λ, no built-in numbers | `(church-add 2 3)` → `5`, `(church-mult 3 4)` → `12`; property: `(church->int (church-add m n))` = `m+n` for random m, n |
| McCarthy (2) | Core `eval`/`apply` loop, S-expression parser, conditional expressions | Eval simple forms: `(+ 1 2)`, `(if #t 1 2)`, `(quote (a b c))`; self-evaluation: interpreter can evaluate its own source |
| Landin (3) | *No code change.* ADR documenting the denotational perspective and how it informs the evaluator's design | ADR reviewed against SICP §4.1 |
| Backus (4) | Point-free combinator module: `compose`, `construct`, `insert`, `apply-to-all` | Property: `(insert + (apply-to-all square '(1 2 3)))` = `14`; verify algebraic laws (e.g., map fusion) |

**Milestone checkpoint:** The interpreter evaluates recursive functions, Church encodings pass all arithmetic properties, combinator laws hold.

#### Phase 2 — Abstraction Power (Tier 2, sources 5–10)

| Source | Feature Added | Tests |
|--------|--------------|-------|
| Sussman & Steele (5) | Lexical closures, proper tail-call elimination | Closure captures: `((let ((x 10)) (lambda (y) (+ x y))) 5)` → `15`; tail-call: mutual recursion `even?`/`odd?` at depth 1M without stack overflow |
| SICP (6) | Streams module (lazy lists), environment model rewrite | `(stream-ref (sieve primes) 100)` returns 547; `(stream-map + nats nats)` = doubled naturals |
| Steele — GOTO (7) | CPS transformer: takes an expression, returns CPS form | Round-trip: `(eval (cps-transform expr))` = `(eval expr)` for a suite of expressions; tree walker in direct style vs. CPS yields identical results |
| Kohlbecker (8) | Hygienic macro expander integrated into eval | `(my-let ((x 1)) x)` → `1`; hygiene: macro-introduced bindings don't capture user bindings (test the classic `or` macro pitfall) |
| Henderson (9) | Lazy evaluation mode: thunks + force/delay in the evaluator | `(define ones (cons 1 (delay ones)))` works; `(take 5 (map + nats ones))` = `(1 3 5 7 9)` |
| Wadler — Monads (10) | Monad protocol (`unit`, `bind`) + maybe, state, list instances | Monad laws (left identity, right identity, associativity) as property tests for each instance; stateful computation: `(run-state (do [x get, _ (put (+ x 1)), y get] y) 0)` → `(1 . 1)` |

**Milestone checkpoint:** The interpreter supports closures, tail calls, CPS, macros, lazy evaluation, and monadic composition. A non-trivial program (e.g., a lazy Sieve of Eratosthenes threaded through the state monad) runs correctly.

#### Phase 3 — Applied FP (Tier 3, sources 11–17, in Clojure)

| Source | Feature Added | Tests |
|--------|--------------|-------|
| Hughes (11) | Port combinator library to Clojure; implement Newton-Raphson and alpha-beta from the paper using only HOFs + lazy seqs | `(take 10 (sqrt-approximations 2))` converges; alpha-beta pruning produces optimal move on known game trees |
| Wadler — Theorems (12) | Parametricity test suite: verify that polymorphic transducer functions obey the predicted theorems | `(= (map f (xf coll)) (xf (map f coll)))` for appropriate `xf`, `f`, `coll` generated randomly |
| Okasaki (13) | `logos.persistent` namespace: banker's queue, leftist heap, red-black tree | Structural invariants as property tests: heap ordering, red-black balance, amortized O(1) queue ops; benchmark against `clojure.data.priority-map` |
| Peyton Jones (14) | ADR comparing Haskell IO with Clojure's ref model; implement a mini STM simulation | STM simulation: concurrent transfers across accounts maintain total invariant under random scheduling |
| Swierstra (15) | `logos.dsl`: composable expression interpreter using protocols + records (expression problem solved) | Add a new operation (e.g., `Mul`) without modifying existing code; all prior tests still pass |
| Kiselyov (16) | Extensible effects library (back in Scheme): `eff-send`, `eff-handle`, delimited continuations | Compose reader + state + exception effects; handler order changes semantics predictably (test both orderings) |
| Hickey (17) | `logos.pvector`: simplified 32-way persistent vector (bit-partitioned trie) | Property: `(= (nth (assoc v i x) i) x)` for random `v`, `i`, `x`; structural sharing: `(identical? (internal-node v) (internal-node (assoc v (count v) :new)))` for unaffected paths |

**Milestone checkpoint:** The Clojure module contains production-grade FP data structures, a composable DSL, and an extensible effects system. All property tests green.


### Code Standards

Every line committed to Λόγος follows these constraints. No exceptions, no "I'll clean it up later."

#### No Comments. Docstrings Only.

Zero `;` comments in the codebase. If something needs explaining, it belongs in a docstring or the code is poorly named.

**Scheme** — every `define` at module level gets a documentation string as its first expression when the form is a `lambda` or the body allows it. For top-level procedures, use a structured docstring convention:

```scheme
(define (church-add m n)
  "λ-encoded addition: applies m successors after n successors.
   m, n : Church numeral (procedure of two arguments)
   returns : Church numeral
   O(m) applications of f"
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
```

**Clojure** — standard metadata docstrings on every public var. No `;` anywhere.

```clojure
(defn persistent-assoc
  "Associates val at index i in persistent vector v.
   Uses 32-way bit-partitioned trie path copying.
   v : PersistentVector
   i : int, 0 <= i <= (count v)
   returns : PersistentVector
   O(log32 n) — effectively O(1) for practical n"
  [v i val]
  ...)
```

#### Docstring Protocol

Every public function documents exactly four things:

1. **What** — one sentence, declarative, no filler
2. **Parameters** — name, type/contract, constraints
3. **Returns** — type/contract
4. **Complexity** — worst-case big-O, and amortized when relevant

Private/helper functions get a one-liner. If the one-liner is hard to write, the function does too much — split it.

#### Naming

- **Predicates**: end in `?` — `empty?`, `balanced?`, `church-zero?`
- **Destructive/stateful** (rare): end in `!` — `swap!`, `reset!`
- **Conversion**: `church->int`, `cps->direct`, `tree->list`
- **Constructors**: `make-env`, `make-thunk`, `->Queue`
- **No abbreviations** except universally known ones (`fn`, `env`, `ctx`, `acc`, `coll`)
- **Namespaces/modules**: singular nouns — `logos.persistent`, `logos.monad`, not `logos.monads`

Names are the primary documentation. A well-named function with a docstring needs nothing else.

#### Complexity Requirements

Every data structure and algorithm in the project must declare and meet its big-O contract.

| Module | Operation | Required Complexity |
|--------|-----------|-------------------|
| Church encodings | `church-add`, `church-mult` | O(m), O(m·n) applications |
| Eval/Apply | environment lookup | O(depth) worst-case; O(1) amortized with hash-based frames |
| CPS transform | transformation pass | O(n) in AST node count |
| Streams | `stream-ref`, `stream-take` | O(k) where k = requested elements |
| Banker's queue | `enqueue`, `dequeue` | O(1) amortized |
| Leftist heap | `insert`, `merge`, `delete-min` | O(log n) |
| Red-black tree | `insert`, `lookup`, `delete` | O(log n) |
| Persistent vector | `nth`, `assoc`, `conj` | O(log₃₂ n) |
| Composable DSL | interpretation | O(n) in expression tree size |
| Extensible effects | `eff-send`, `eff-handle` | O(k) where k = handler chain depth |

Any function that exceeds its stated complexity is a bug. Property tests should include performance assertions for critical paths (e.g., queue operations on 10⁶ elements complete in bounded time).

#### Code Shape

**Max line length**: 80 characters. No exceptions.

**Max function body**: 15 lines excluding docstring. If longer, decompose. The evaluator's `eval-dispatch` may use a `cond` that exceeds this — that is the one permitted exception, and only because a dispatch table is inherently a flat enumeration.

**No `let` nesting beyond 2 levels.** Deep nesting means missing abstractions. Extract a named helper.

**Threading macros in Clojure** (`->`, `->>`, `some->`) are preferred over nested calls when the pipeline has ≥3 stages.

**Destructuring over positional access.** `(let [{:keys [left right]} node] ...)` not `(let [l (:left node) r (:right node)] ...)`.

**Pure functions by default.** Any function performing I/O or mutation must:
1. Be suffixed with `!`
2. Live at the boundary — never called from pure functions
3. Be documented as effectful in its docstring

#### Formatting

**Scheme**: follow the conventions of *The Scheme Programming Language* (Dybvig). `cond` clauses aligned, `let` bindings vertically aligned, closing parens never on their own line.

**Clojure**: `cljfmt` defaults. Metadata before arglist. `ns` form sorted: `:require` alphabetical, no `:use`.

#### Git Discipline

- One commit per source/concept. Message format: `{source-number}: {concept} — {what changed}`
  Example: `07: CPS transform — add cps.ss with transformer and round-trip tests`
- Tests and implementation in the same commit. Never commit code without its tests.
- Reflections in a separate commit: `07: reflection — CPS and the continuation-passing style`
- ADRs committed when the decision is made, not retroactively.


### Testing Strategy

Three layers, applied consistently across both modules.

1. **Example tests** — golden inputs/outputs for each feature, documenting expected behavior. Minimum 5 per source.
2. **Property tests** — algebraic laws and invariants. Use `(srfi 64)` + custom generators in Scheme, `test.check` in Clojure. Every algebraic structure (monad, functor, monoid) gets its law suite.
3. **Self-hosting test** — at the end of Phase 2, the Scheme interpreter must evaluate a copy of its own `eval` and produce correct results. This is the single most demanding correctness check.

Test naming convention: `{source-number}-{concept}-{what-is-verified}`
Example: `02-mccarthy-eval-recursive-factorial`, `10-wadler-monad-left-identity`


### Integration with Study Method

The per-source protocol from the Method section now has a fifth step:

1. First reading
2. Implementation → commit to `logos/`
3. Tests → committed alongside, CI green
4. Reflection → `docs/reflections/{nn}-{author}.org`
5. Spaced review — re-run tests + re-read reflection at 7d, 30d, 90d


## What This Spec Does Not Cover

- Type theory deep dives (MLF, HoTT) — out of scope unless Tier 3 pulls you there
- Production Clojure patterns (spec, component, integrant) — that's work, not study
- Web/distributed systems FP — orthogonal concern
