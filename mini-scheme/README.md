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
