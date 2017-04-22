[![Build Status](https://travis-ci.org/philnguyen/z3-rkt.svg?branch=master)](https://travis-ci.org/philnguyen/intern) intern
=========================================

Library for type-safe interning of a type as new subtype of `Index` in Typed Racket.
This is useful when some (sub-)datatypes are carried around and compared for equality
much more often than they are created
(for example, the `address` component in [AAM](http://dl.acm.org/citation.cfm?id=1863553)-style static analysis).

### Install

```
raco pkg install intern
```

### Example

The following example of interning a `Setof String`
explicitly provides the default names to all optional arguments.


```racket
#lang typed/racket/base
(require racket/set intern)

(define-interner String-Set (Setof String)
  #:intern-function-name String-Set-of
  #:unintern-function-name of-String-Set)
;; Generates:
;;   String-Set-of : (Setof String) → String-Set
;;   of-String-Set : String-Set → (Setof String)
;;   count-String-Set : → Index

(define s₁ (String-Set-of (set "foo" "bar")))
(define s₂ (String-Set-of (set-add (of-String-Set s₁) "qux")))
(define s₃ (String-Set-of (set-add (of-String-Set s₂) "qux")))
(count-String-Set) ; ==> #t
(equal? s₂ s₃) ; ==> #t
```