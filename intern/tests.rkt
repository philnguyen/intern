#lang typed/racket/base

(require racket/set
         typed/rackunit
         "main.rkt")

(define-interner String-Set (Setof String)
  #:intern-function-name String-Set-of
  #:unintern-function-name of-String-Set)

(define s₁ (String-Set-of (set "foo" "bar")))
(define s₂ (String-Set-of (set-add (of-String-Set s₁) "qux")))
(define s₃ (String-Set-of (set-add (of-String-Set s₂) "qux")))
(check-equal? (count-String-Set) 2)
(check-equal? s₂ s₃)
