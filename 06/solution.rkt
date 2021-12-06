#lang racket/base

(require racket/list
         racket/sequence
         racket/string
         racket/vector)

(define (sum seq)
  (sequence-fold
    (lambda (s i) (+ s i))
    0
    seq))

(define (input)
  (define (make-counters s)
    (sequence-fold
     (lambda (counters current-value)
       (hash-set!
        counters
        current-value
        (+ (hash-ref counters current-value 0) 1))
       counters)
     (make-hash)
     (map
      string->number
      (string-split s ","))))
  (define (counters-to-list counters)
    (list->vector
     (map
      (lambda (i) (hash-ref counters i 0))
      (range 9))))
  (with-input-from-file "input.txt"
     (lambda ()
       (counters-to-list
         (make-counters
           (read-line))))))

(define (step v)
  (build-vector
    9
    (lambda (i)
      (cond ((= i 8) (vector-ref v 0))
            ((= i 6) (+ (vector-ref v 7) (vector-ref v 0)))
            (else (vector-ref v (+ i 1)))))))

(define (steps lst count)
  (define (iter lst i)
    (if (= i count)
        lst
        (iter (step lst) (+ i 1))))
  (iter lst 0))

(display "Task 1: ")
(display (sum (steps (input) 80)))
(newline)

(display "Task 1: ")
(display (sum (steps (input) 256)))
(newline)
