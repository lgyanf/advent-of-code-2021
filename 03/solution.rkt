#lang racket/base

(require racket/sequence
         racket/struct)

(define (zip l1 l2) (map list l1 l2))

(define (binary->number bin)
  (define (parse-binary-char ch)
    (cond ((or (equal? ch #\0) (equal? ch 0)) 0)
          ((or (equal? ch #\1) (equal? ch 1)) 1)
          (else (error "Bad binary char" ch))))
  (define (iter-reversed reversed-bin current-power-of-2 acc)
   (if (null? reversed-bin)
       acc
       (iter-reversed
        (cdr reversed-bin)
        (* current-power-of-2 2)
        (+ acc (* (parse-binary-char (car reversed-bin)) current-power-of-2)))))
  (let ((bin-lst (cond ((string? bin) (string->list bin))
                       (else bin))))
    (iter-reversed (reverse bin-lst) 1 0)))

(struct bit-counter (zeros ones)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
       (lambda (obj) 'bit-counter)
       (lambda (obj) (list (bit-counter-zeros obj) (bit-counter-ones obj)))))])

(define (make-bit-counter ch)
    (cond ((equal? ch #\0) (bit-counter 1 0))
          ((equal? ch #\1) (bit-counter 0 1))
          (else (error "Bad binary char" ch))))

(define (merge-bit-counters c1 c2)
  (bit-counter
    (+ (bit-counter-zeros c1) (bit-counter-zeros c2))
    (+ (bit-counter-ones c1) (bit-counter-ones c2))))

(define (merge-bit-counter-pair p)
  (let ((c1 (car p))
        (c2 (cadr p)))
    (merge-bit-counters c1 c2)))

(define (gamma counter)
  (if (> (bit-counter-zeros counter) (bit-counter-ones counter))
      0
      1))

(define (epsilon counter)
  (- 1 (gamma counter)))

    
(define (count binary-string)
  (define (iter binary-string acc)
    (if (null? binary-string)
        acc
        (iter (cdr binary-string) (cons (make-bit-counter (car binary-string)) acc))))
  (reverse (iter (string->list binary-string) '())))

(define (input)
  (with-input-from-file "input.txt"
    (lambda ()
      (sequence->list (in-lines)))))

(define (count-bits-in-binary-string-list binary-strings)
  (sequence-fold
    (lambda (acc binary-string)
      (map
        merge-bit-counter-pair
        (zip acc (count binary-string))))
    (count (car binary-strings))
    (cdr binary-strings)))

(define (bit-counter-process in make-criterion)
  (define (iter in position)
    (let ((counts (list->vector (count-bits-in-binary-string-list in))))
      (if (equal? (cdr in) '())
          (car in)
          (iter
           (filter (make-criterion counts position) in)
           (+ position 1)))))
  (iter in 0))

(define (make-oxygen-generator-criterion counts position)
  (let ((counter (vector-ref counts position)))
    (if (> (bit-counter-zeros counter) (bit-counter-ones counter))
        (lambda (s) (equal? (string-ref s position) #\0))
        (lambda (s) (equal? (string-ref s position) #\1)))))

(define (make-co2-scrubber-criterion counts position)
  (let ((counter (vector-ref counts position)))
    (if (< (bit-counter-ones counter) (bit-counter-zeros counter))
        (lambda (s) (equal? (string-ref s position) #\1))
        (lambda (s) (equal? (string-ref s position) #\0)))))

(let ((in (input)))
  (let ((counts (count-bits-in-binary-string-list in)))
    (let ((g (binary->number (map gamma counts)))
          (e (binary->number (map epsilon counts)))
          (o2 (binary->number (bit-counter-process in make-oxygen-generator-criterion)))
          (co2 (binary->number (bit-counter-process in make-co2-scrubber-criterion))))
      (display "Counters:")
      (display counts)
      (newline)
      (display "Gamma: ")
      (display g)
      (newline)
      (display "Epsilon: ")
      (display e)
      (newline)
      (display "Task 1 answer: ")
      (display (* g e))
      (newline)
      (display "O2: ")
      (display o2)
      (newline)
      (display (bit-counter-process in make-oxygen-generator-criterion))      
      (newline)
      (display "CO2: ")
      (display co2)
      (newline)
      (display "Task 2 answer: ")
      (display (* o2 co2))
      (newline))))