#lang typed/racket/base

(provide define-interner)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define-syntax define-interner
  (syntax-parser
    [(_ T
        #:as T*:id
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
         ;; Private
         (define m   : (HashTable T  T*) (mk-m))
         (define m⁻¹ : (HashTable T* T ) (make-hasheq))
         
         ;; Public
         (define-new-subtype T* (->T* Index))
         
         (: intern : T → T*)
         (define (intern t)
           (cond [(hash-ref m t #f) => values]
                 [else
                  (define t* (->T* (hash-count m)))
                  (hash-set! m   t  t*)
                  (hash-set! m⁻¹ t* t )
                  t*]))
         
         (: unintern : T* → T)
         (define (unintern t*)
           (hash-ref m⁻¹ t* (λ () (error 'unintern "nothing at ~a" t*))))

         (define (count-T*) (hash-count m⁻¹)))]))
