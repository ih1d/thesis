# Mini-Scheme Interpreter

A subset of Scheme implemented in Haskell with different effect systems.

## Purpose

The interpreter is implemented in the following effect systems:

- **mtl** (monad transformers)
- **fused-effects**
- **freer-simple**
- **polysemy**
- **effectful**

The purpose is to directly compare:

- Code ergonomics
- Performance
- Expressiveness (especially around effect interaction)

between the 5 effect systems

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

## Prerequisites && Building

[Download the haskell ecosystem](https://www.haskell.org/ghcup/).

Then clone this directory:

```sh
git init mini-scheme
cd mini-scheme
git remote add -f origin https://github.com/ih1d/thesis
git config core.sparseCheckout true
echo "mini-scheme" >> .git/info/sparse-checkout
git pull origin main
```

Then build and run:

```sh
cabal build
cabal run <backend> # mtl, fused-effects, freer-simple, polysemy, effectful
```
