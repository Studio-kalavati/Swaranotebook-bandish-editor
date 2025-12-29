# Agent Guidelines for bandish-editor

## Project Overview

This is a ClojureScript (re-frame) music editor for Indian classical music bandishes.
Frontend: ClojureScript + Re-frame + Re-com, Build: shadow-cljs, Tests: cljs-test + Karma

---

## Commands

### Build & Development
- `npm run dev` - Start shadow-cljs watch for app (default port 8280)
- `npm run release` - Production build (optimizations: simple)
- `npm run build-report` - Generate build report at target/build-report.html
- `npm run watch` - Watch app, browser-test, and karma-test builds

### Testing
- `npm run test` - Run all node tests (shadow-cljs compile test && node out/node-tests.js)
- To run a **single test file**: Temporarily comment out other tests in test/bhatkhande_editor/*_test.cljs
- To run a **single test**: Comment out other `deftest` forms in the relevant test file
- `npm run test:puppeteer` - Run Puppeteer end-to-end tests
- `npm run dev-and-test` - Run dev server and puppeteer tests concurrently

### Linting
- `cd functions && npx eslint .` - Lint JavaScript/Functions codebase
- No explicit linter for ClojureScript code

---

## Code Style Guidelines

### Formatting
- **Indentation**: 2 spaces (Clojure/ClojureScript convention)
- **Line length**: No strict limit, but prefer < 100 chars when practical
- **Trailing whitespace**: Remove all trailing whitespace

### Imports / Requires
```clojure
(ns bhatkhande-editor.module
  (:require
    [reagent.core :as reagent]
    [re-frame.core :as re-frame
     :refer [reg-event-db reg-event-fx dispatch]]
    [clojure.string :as str]
    [bhatkhande-editor.db :as db :refer [some-fn]]
    [some.library :as lib]))
```
- Group requires logically: framework libs first, then internal modules
- Use `:as` alias for common imports
- Use `:refer` only for heavily used functions (< 5-6 imports per ns)
- Use namespaced keywords (`::event-name`) for re-frame events

### Naming Conventions
- **Functions**: kebab-case (`cursor2vec`, `update-noteseq`)
- **Variables**: kebab-case (`cursor-pos`, `next-cursor`)
- **Constants**: kebab-case with kebab-case words (`cursor-index-keys`, `default-db`)
- **Re-frame events**: namespaced keywords (`::conj-svara`, `::set-raga`)
- **Predicates**: end with `?` (`running-on-localhost?`, `newsletter-signup?`)
- **Private defs**: `def ^:private` when appropriate
- **Parameters**: destructured maps (`{:keys [db]} [_ params]`)

### Re-frame Event Handlers
```clojure
(reg-event-fx
  ::event-name
  [log-event]  ; optional interceptors
  (fn [{:keys [db]} [_ param1 param2]]
    {:db (assoc db :key value)
     :dispatch [::other-event]}))

(reg-event-db
  ::simple-event
  (fn [db [_ param]]
    (assoc db :key param)))
```
- Use `reg-event-fx` when needing side effects (dispatch, http, firebase, etc.)
- Use `reg-event-db` for pure db updates
- Extract handler functions to separate defs if complex (> 10 lines)
- Use interceptors for logging (`log-event`), clearing state (`clear-highlight-interceptor`)

### Error Handling
```clojure
(try
  (some-expr)
  (catch js/Error e
    (println "Error:" e)
    {}))  ; return empty effects or error state
```
- Wrap async operations in try-catch (fetch, firebase, file I/O)
- Log errors with `println` for development
- Return safe default values or error state maps
- Use `(when config/debug? (println ...))` for debug logging

### Comments
- No inline comments unless explaining complex logic
- Function docstrings optional, but describe complex transformations
- No comment blocks at top of files unless explaining architecture

### Interop with JavaScript
```clojure
;; Call JS methods
(.method js-object arg1 arg2)
(.set! (.-prop js-object) value)

;; Create JS objects
(clj->js {:key val})
(js->clj js-obj)
```

### Spec / Validation
- Use clojure.spec.alpha for data validation
- Specs defined in utils.cljs (`:snb/svara`, `snb/composition`, etc.)
- Validate JSON on import with `s/valid?` and throw with `s/explain` on failure

### Testing
```clojure
(ns bhatkhande-editor.module-test
  (:require [cljs.test :refer (deftest is)]
            [bhatkhande-editor.module :as sut]))

(deftest function-name-test
  (is (= expected (sut/function-name input))))
```
- Test files in `test/bhatkhande_editor/` with `*_test.cljs` suffix
- Use `deftest` and `is` from cljs.test
- Assert values directly, avoid side effects in tests
- Namespace variable under test as `sut` (system under test)

### Destructuring Patterns
- For event handlers: `{:keys [db]} [_ param1 param2]]`
- For map destructuring: `{:keys [key1 key2] :or {key1 default}}`
- For vectors: `[[a b c :as vec] (some-func)]`

### Constants & Config
- Define constants in db.cljs for shared values (`pitch-s-list`, `cursor-index-keys`)
- Define build config in config.cljs (`debug?`)
- Use `defonce` for one-time initialization (`firebase-app-info`)

### Interceptors
- Define interceptors with `re-frame.core/->interceptor`
- Interceptor pattern: `:id` (keyword), `:after` or `:before` (fn)
- Common use cases: logging, clearing state, analytics

### File Organization
- `core.cljs` - App initialization, mounting, Firebase setup
- `events.cljs` - Re-frame event handlers
- `subs.cljs` - Re-frame subscriptions
- `views.cljs` - Reagent components
- `db.cljs` - Initial db state, helper functions
- `utils.cljs` - Utility functions, specs, JSON parsing
- `routes.cljs` - Client-side routing (bidi/pushy)

---

## Notes

- This is a ClojureScript project using shadow-cljs for compilation
- re-frame is used for state management (unidirectional data flow)
- Firebase for backend (auth, storage, firestore)
- Audio API for sound playback (santoor, tabla, tanpura samples)
- Browser devtools must have "Disable cache" and "Enable custom formatters" enabled
- For single test runs: Comment out other tests in test files (no built-in filter for shadow-cljs test runner)

# Clojure REPL Evaluation

The command `clj-nrepl-eval` is installed on your path for evaluating Clojure code via nREPL.

**Discover nREPL servers:**

`clj-nrepl-eval --discover-ports`

**Evaluate code:**

`clj-nrepl-eval -p <port> "<clojure-code>"`

With timeout (milliseconds)

`clj-nrepl-eval -p <port> --timeout 5000 "<clojure-code>"`

The REPL session persists between evaluations - namespaces and state are maintained.
Always use `:reload` when requiring namespaces to pick up changes.
