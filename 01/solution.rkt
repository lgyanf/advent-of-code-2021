#lang racket/base

(require algorithms)
(require racket/list)
(require racket/sequence)

(define (read-numbers filename)
  (with-input-from-file filename
    (lambda ()
      (map string->number (sequence->list (in-lines))))))

(define (count-decreases depths)
  (car 
   (sequence-fold
    (lambda (acc current)
      (let ((count (car acc))
            (prev (cdr acc)))
        (if (> current prev)
            (cons (+ count 1) current)
            (cons count current))))
    (cons 0 (sequence-ref depths 0))
    (sequence-tail depths 1))))

(define (sliding-window-3-sum nums)
  (let ((result (make-vector (- (length nums) 2))))
    (define (iter nums a b c position)
      (vector-set! result position (+ a b c))
      (if (null? nums)
        result
        (iter (cdr nums) b c (car nums) (+ position 1))))
    (iter (cdddr nums) (car nums) (cadr nums) (caddr nums) 0)))

(define (input) (read-numbers "input.txt"))

(display "Task 1: ")
(display (count-decreases (input)))
(newline)

(display "Task 2v1: ")
(display (count-decreases (sliding-window-3-sum (input))))
(newline)

(display "Task 2v2: ")
(display (count-decreases (map sum (sliding (input) 3 1))))