#lang racket/base

(require racket/sequence)

(define (count-decreases depths)
  (define (iter depths last-depth acc)
    (if (null? depths)
        acc
        (let ((depth (car depths)))
          (iter (cdr depths) depth (if (> depth last-depth) (+ acc 1) acc)))))
  (if (null? depths) 0 (iter (cdr depths) (car depths) 0)))

(define (count-decreases-from-file filename)
  (with-input-from-file filename
    (lambda ()
      (count-decreases (map string->number (sequence->list (in-lines)))))))

(count-decreases-from-file "input.txt")
