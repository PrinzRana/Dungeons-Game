#lang racket

(provide literal var seq nothing alt eof)

(define (match parser str)
  (parser str 0 '() (λ (env index) env) (λ () #f)))

(define (literal text)
  (λ (str index env succ fail)
    (match-literal text 0 str (skip-whitespace str index) env succ fail)))

(define (match-literal literal i1 text i2 env succ fail)
  (if (eq? (string-length literal) i1)
      (succ env i2)
      (if (eq? (string-length text) i2)
          (fail)
          (if (eq? (string-ref literal i1) (string-ref text i2))
              (match-literal literal (+ i1 1) text (+ i2 1) env succ fail)
              (fail)))))

(define (var name)
  (λ (str index env succ fail)
    (let ((index (skip-whitespace str index)))
      (if (eq? index (string-length str))
          (fail)
          (let-values
              (([value index] (next-literal str index)))
            (succ `((,name . ,(list->string value)) . ,env) index))))))

(define (skip-whitespace str index)
  (if (eq? (string-length str) index)
      index
      (if (whitespace? (string-ref str index))
          (skip-whitespace str (+ index 1))
          index)))

(define (whitespace? char)
  (cond ((eq? #\space char) #t)
        (else #f)))

(define (next-literal str index)
  (define (chars index)
    (if (or (eq? index (string-length str)) (whitespace? (string-ref str index)))
        '()
        (cons (string-ref str index) (chars (+ index 1)))))
  (let ((cs (chars index)))
    (values cs (+ index (length cs)))))

(define (seq . parsers)
  (define (seq2 first second)
    (λ (str index env succ fail)
      (first str index env (λ (env index) (second str index env succ fail)) fail)))
  (foldr seq2 nothing parsers))

(define (nothing str index env succ fail)
  (succ env index))

(define (fail str index env succ fail)
  (fail))

(define (alt . parsers)
  (define (alt2 alt1 alt2)
    (λ (str index env succ fail)
      (alt1 str index env succ (λ () (alt2 str index env succ fail)))))
  (foldr alt2 fail parsers))

(define (eof str index env succ fail)
  (if (eq? (string-length str) (skip-whitespace str index))
      (succ env (skip-whitespace str index))
      (fail)))

(define (test1)
  (match (literal "test1") "test1"))

(define (test2)
  (match (literal "test1") "test2"))

(define (test3)
  (match (var 'x) "   abc   "))

(define (test4)
  (match (seq (literal "xxx") (var 'x) (var 'y))))

(define (test5)
  (match (alt (literal "xxx") (literal "yyy")) "  xxx"))

(define (test6)
  (match (alt (literal "xxx") (literal "yyy")) "  yyy"))

(define (test7)
  (match (alt (literal "xxx") (literal "yyy")) "  aaa"))

(define graph '((1 2) (2 3) (3 4)))

(define (flatten g)
  (cond ((equal? g '()) (set))
        (#t (set-union (flatten (rest g)) (list->set (first g))))))