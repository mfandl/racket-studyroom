#lang racket

(define-syntax-rule (swap x y) (let ([tmp x])
                                 (set! x y)
                                 (set! y tmp)))

(define (swap-fn x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define-syntax rotate
  (syntax-rules ()
    [(rotate a c ...)
     (shift-to (c ... a) (a c ...))]))

(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (from0 from ...) (to0 to ...))
     (let ([tmp from0])
       (displayln (list from0 to0))
       (set! to from) ...
       (set! to0 tmp))]))

(define-syntax val
  (lambda (stx)
    (syntax-case stx ()
      [val (identifier? (syntax val)) (syntax (get-val))])))

(define-values (get-val put-val!)
  (let ([private-val 0])
    (values (lambda () private-val)
            (lambda (v) (set! private-val v)))))

(define-syntax val2
  (make-set!-transformer
    (lambda (stx)
      (syntax-case stx (set!)
        [val2 (identifier? (syntax val2)) (syntax (get-val))]
        [(set! val2 e) (syntax (put-val! e))]))))

(define-syntax-rule (define-get/put-id id get put!)
  (define-syntax id
    (make-set!-transformer
      (lambda (stx)
        (syntax-case stx (set!)
          [id (identifier? (syntax id)) (syntax (get))]
          [(set! id e) (syntax (put! e))])))))

(define-syntax-rule (define-cbr (id arg ...) body)
  (begin
    (define-syntax id
      (syntax-rules ()
        [(id actual (... ...))
         (do-f (lambda () actual)
               (... ...)
               (lambda (v)
                 (set! actual v))
               (... ...))]))
    (define-for-cbr do-f (arg ...)
                    ()
                    body)))

(define-syntax define-for-cbr
  (syntax-rules ()
    [(define-for-cbr do-f (id0 id ...)
                     (gens ...) body)
     (define-for-cbr do-f (id ...)
                     (gens ... (id0 get put)) body)]
    [(define-for-cbr do-f ()
                     ((id get put) ...) body)
     (define (do-f get ... put ...)
       (define-get/put-id id get put) ...
       body)]))

(define-values (a b c d) (values 1 2 3 4))

(define-values (a1 b1 c1 d1) (values 1 2 3 4))

(swap a b)
(swap-fn c d)
(displayln (list a b c d))

(rotate a1 b1 c1 d1)
(rotate a1 b1 c1 d1)

(displayln (list a1 b1 c1 d1))
(displayln `(,val ,(+ val 3)))

(set! val2 100)
(displayln val2)

(define-get/put-id val3 get-val put-val!)
(set! val3 11)
(displayln val3)
(displayln val2)


(define-cbr (q u i)
            (swap u i))

(displayln (let ([x 1] [y 2])
             (q x y)
             (list x y)))

(define-syntax strange-sum
  (syntax-rules ()
    [(strange-sum a b ...)
     (list (- a b) ...)]))

(define-syntax strange-sum2
  (syntax-rules ()
    [(strange-sum2 a) a]
    [(strange-sum2 a b ...)
     (+ a (strange-sum2 b ...))]))

(define-syntax strange-sum3
  (syntax-rules ()
    [(strange-sum3 (a b ...) (c d ...))
     (list (begin (displayln (list a c b)) (+ a c b)) ... (begin (displayln (list b d a)) (+ b d a)) ...)]))

(displayln (strange-sum 2 3 4 5 6 7))
(displayln (strange-sum2 2 3 4 5 6))
(displayln '((1 2 3) (7 8 9)))
(displayln (strange-sum3 (1 2 3) (7 8 9)))


(define-values (m n) (values 2 3))

(display (begin (q m n) (list m n)))
