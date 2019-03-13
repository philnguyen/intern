#lang typed/racket/base

(provide define-interner)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(: make-interner (∀ (X X*)
                    (Index → X*)
                    (→ (Mutable-HashTable X X*))
                    Symbol
                    →
                    (Values (X → X*)
                            (X* → X)
                            (→ Index))))
(define (make-interner mk-tag mk-m unintern-error-tag)
  (define m   : (Mutable-HashTable X X*) (mk-m))
  (define m⁻¹ : (Mutable-HashTable X* X) (make-hasheq))

  (values
   ;; intern
   (λ (x)
     (cond [(hash-ref m x #f) => values]
           [else
            (define x* (mk-tag (hash-count m)))
            (hash-set! m   x x*)
            (hash-set! m⁻¹ x* x)
            x*]))
   ;; unintern
   (λ (x*)
     (hash-ref m⁻¹ x* (λ () (error unintern-error-tag "nothing at ~a" x*))))
   ;; count
   (λ () (hash-count m⁻¹))))

(define-syntax define-interner
  (syntax-parser
    [(_ T*:id T
        (~optional (~seq #:intern-function-name intern:id)
                   #:defaults ([intern (format-id #'T "~a-of" (syntax-e #'T*))]))
        (~optional (~seq #:unintern-function-name unintern:id)
                   #:defaults ([unintern (format-id #'T "of-~a" (syntax-e #'T*))])))
     (define T.eq?
       (syntax-parse #'T
         [(~or (~literal Index) (~literal Fixnum) (~literal Symbol) (~literal Char))
          #t]
         [_ #f]))
     (define/with-syntax count-T* (format-id #'T "count-~a" (syntax-e #'T*)))
     (define/with-syntax mk-m (if T.eq? #'make-hasheq #'make-hash))
     #'(begin
         (define-new-subtype T* (->T* Index))
         (define-values (intern unintern count-T*) ((inst make-interner T T*) ->T* mk-m 'unintern)))]))
