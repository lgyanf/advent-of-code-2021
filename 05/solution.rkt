#lang racket/base

(require racket/hash
         racket/list
         racket/sequence
         racket/string
         racket/struct)

(define (flatmap f lst)
  (apply append (map f lst)))

(struct point (x y)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
       (lambda (obj) 'point)
       (lambda (obj) (list (point-x obj) (point-y obj)))))])
  
(struct line (p1 p2)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
       (lambda (obj) 'line)
       (lambda (obj) (list (line-p1 obj) (line-p2 obj)))))])  

(define (sign x)
  (if (< x 0)
      -1
      1))

(define (get-minimal-step line)
  (let ((p1 (line-p1 line))
        (p2 (line-p2 line)))
    (let ((dx (- (point-x p2) (point-x p1)))
          (dy (- (point-y p2) (point-y p1))))
      (cond ((and (= dx 0) (= dy 0)) (error "Malformed line"))
            ((= dx 0) (cons 0 (sign dy)))
            ((= dy 0) (cons (sign dx) 0))
            (else
              (let ((g (gcd (abs dx) (abs dy))))
                (cons (/ dx g) (/ dy g))))))))

(define (vertical? line)
  (let ((step (get-minimal-step line)))
    (= (car step) 0)))

(define (horizontal? line)
  (let ((step (get-minimal-step line)))
    (= (cdr step) 0)))

(define (diagonal? line)
  (let ((step (get-minimal-step line)))
    (and
      (= (abs (car step)) 1)
      (= (abs (cdr step)) 1))))

(define (line-points line)
  (let ((step (get-minimal-step line))
        (start (line-p1 line))
        (end (line-p2 line)))
    (define (iter position acc)
      (if (equal? position end)
          (cons position acc)
          (iter
            (point (+ (point-x position) (car step))
                   (+ (point-y position) (cdr step)))
            (cons position acc))))
    (iter (line-p1 line) '())))
            

(define (parse-line s)
  (define (parse-number-robust s)
    (string->number (string-trim s)))
  (define (parse-point coords)
    (let ((coords (map parse-number-robust coords)))
       (point (car coords) (cadr coords))))
  (let ((coord-pairs (map
                       (lambda (s) (string-split s ","))
                       (string-split s "->"))))
    (line (parse-point (car coord-pairs)) (parse-point (cadr coord-pairs)))))

(define (input)
  (with-input-from-file "input.txt"
    (lambda ()
      (sequence->list (sequence-map parse-line (in-lines))))))

(define (count-overlaps line-filter)
  (let ((field (make-hash)))
    (let ((lines (filter line-filter (input))))
      (let ((points (flatmap line-points lines)))
        (for-each
         (lambda (point)
           (hash-set!
            field
            point
            (+ (hash-ref field point 0) 1)))
         points)
        (length
         (filter
          (lambda (x) (> x 1))
          (hash-values field)))))))

(display "Task 1: ")
(display
  (count-overlaps
    (lambda (line)
      (or
        (horizontal? line)
        (vertical? line)))))
(newline)
(display "Task 2: ")
(display
  (count-overlaps
    (lambda (line)
      (or
        (horizontal? line)
        (vertical? line)
        (diagonal? line)))))
(newline)
