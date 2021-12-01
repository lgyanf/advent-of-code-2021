#lang racket/base

(require racket/sequence)
(require racket/list)

(define (read-numbers filename)
  (with-input-from-file filename
    (lambda ()
      (map string->number (sequence->list (in-lines))))))

(define (count-decreases depths)
  (define (iter depths last-depth acc)
    (if (null? depths)
        acc
        (let ((depth (car depths)))
          (iter (cdr depths) depth (if (> depth last-depth) (+ acc 1) acc)))))
  (if (null? depths) 0 (iter (cdr depths) (car depths) 0)))

(define nil '())

(define (sliding-window-3-sum nums)
  (let ((result (make-vector (- (length nums) 2))))
    (define (iter nums a b c position)
      (vector-set! result position (+ a b c))
      (if (null? nums)
        (vector->list result)
        (iter (cdr nums) b c (car nums) (+ position 1))))
    (iter (cdddr nums) (car nums) (cadr nums) (caddr nums) 0)))

(define (input) (read-numbers "input.txt"))

(display "Task 1: ")
(display (count-decreases (input)))
(newline)

(display "Task 2: ")
(display (count-decreases (sliding-window-3-sum (input))))
(newline)