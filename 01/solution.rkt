#lang racket/base

(require racket/sequence)

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

(define (sliding-window-3-sum nums)
  (define (iter nums a b c acc)
    (if (null? nums)
        acc
        (iter b c (car nums) (append acc (list (+ a b c))))))
  (iter (cddr nums) (car nums) (cadr nums) (caddr nums) '()))

(define (input) (read-numbers "input.txt"))

(display "Task 1: ")
(display (count-decreases (input)))
(newline)
(sliding-window-3-sum (input))

(display "Task 2: ")