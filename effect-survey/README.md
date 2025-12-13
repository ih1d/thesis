# Mini-Scheme Interpreter

A minimal Scheme-like interpreter implemented with multiple Haskell effect libraries for comparison.

## Purpose

This interpreter serves as a testbed for comparing effect system implementations:

- **MTL** (monad transformers)
- **fused-effects**
- **freer-simple**
- **polysemy**
- **effectful**

The same language semantics are implemented in each library, allowing direct comparison of:

- Code ergonomics
- Performance
- Expressiveness (especially around effect interaction)

## Language Features

```scheme
; Literals
42              ; integers
#t #f           ; booleans
"hello"         ; strings

; Variables
(define x 10)   ; global definition
(set! x 20)     ; mutation
x               ; lookup

; Arithmetic
(+ 1 2)         ; addition
(- 10 5)        ; subtraction
(* 3 4)         ; multiplication
(/ 10 2)        ; division (throws on /0)

; Comparison
(= x 10)        ; equality
(< x 20)        ; less than
(> x 5)         ; greater than

; Control flow
(if (< x 10) "small" "big")

; Local binding (tests higher-order effects!)
(let ((x 1) (y 2)) 
  (+ x y))

; Sequencing
(begin
  (define x 1)
  (set! x 2)
  x)

; Error handling (tests error+state interaction!)
(try (/ 1 0) 999)    ; catch error, return fallback
(fail "message")     ; throw error

; Logging
(log "checkpoint")   ; writes to log output

; Meta commands
(history)            ; show command history
(env)                ; show current bindings
(reset)              ; clear environment
(quit)               ; exit REPL
```

## Effects Used

| Effect | Purpose | Key Test |
|--------|---------|----------|
| Reader | Lexical environment | `let` scoping via `local` |
| State | Global definitions, history | Mutation semantics |
| Writer | Logging | `log` command |
| Error | Exception handling | `try`/`fail`, division by zero |

## Building

```bash
cabal build all
```

## Running

```bash
# Default (MTL backend)
cabal run mini-scheme

# Specific backend
cabal run mini-scheme -- mtl
cabal run mini-scheme -- fused-effects
cabal run mini-scheme -- freer-simple
cabal run mini-scheme -- polysemy
cabal run mini-scheme -- effectful
```

## Testing

```bash
# Parser tests
cabal test parser-test
```

## Key Research Questions

### 1. State + Error Interaction

```scheme
(begin
  (define x 0)
  (try 
    (begin (set! x 1) (/ 1 0))
    999)
  x)  ; Is x 0 or 1?
```

Does catching an error preserve or rollback state changes?

### 2. Higher-Order Effects (`local`)

```scheme
(define x 10)
(let ((x 1)) 
  (let ((y 2)) 
    (+ x y)))  ; Must correctly restore x=10 after
```

Does `local` (used by `let`) work correctly with all libraries?

### 3. Nested Error Handlers

```scheme
(try
  (try (/ 1 0) (fail "inner"))
  "outer")  ; Should this return "outer"?
```

How do nested `try` blocks interact?

## Project Structure

```sh
mini-scheme/
├── src/
│   ├── Types.hs           -- AST, values, environment
│   ├── Lexer.hs           -- Hand-written lexer
│   ├── Parser.hs          -- Hand-written recursive descent parser
│   └── Eval/
│       ├── MTL.hs         -- MTL implementation
│       ├── FusedEffects.hs
│       ├── FreerSimple.hs
│       ├── Polysemy.hs
│       └── Effectful.hs
├── app/
│   └── Main.hs            -- REPL with backend selection
├── test/
│   └── ParserTest.hs      -- Parser tests
└── mini-scheme.cabal
```

## Implementation Notes

The parser is intentionally dependency-free (no parsec, megaparsec, etc.) to avoid any effect system contamination in the comparison.

Each evaluator module exports the same interface:

- `eval :: Expr -> <EffectMonad> Value`
- `runEval :: EvalState -> String -> Expr -> (Either EvalError Value, [String], EvalState)`
- `initialState :: EvalState`