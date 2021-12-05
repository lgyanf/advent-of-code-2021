#lang racket/base

(require algorithms
         racket/list
         racket/sequence
         racket/string
         racket/struct)
                       
(struct board-cell (number [is-marked #:mutable])
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'number)
      (lambda (obj) (list (board-cell-number obj) (board-cell-is-marked obj)))))])
(define (make-board-cell number) (board-cell number #f))

(struct board (rows))

(define (mark board value)
  (sequence-for-each
   (lambda (row)
     (sequence-for-each
      (lambda (cell) (set-board-cell-is-marked! cell #t))
      (sequence-filter
       (lambda (cell) (equal? (board-cell-number cell) value))
       row)))
   (board-rows board)))

(define (is-winning board)
  (define (is-row-winning row)
    (sequence-andmap board-cell-is-marked row))
  (define (is-col-winning col)
    (sequence-andmap
     (lambda (row) (board-cell-is-marked (vector-ref row col)))
     (board-rows board)))
  (or
   (sequence-ormap is-row-winning (board-rows board))
   (sequence-ormap is-col-winning (range 5))))

(define (print-board board)
  (define (print-row row)
    (display row)
    (newline))
  (sequence-for-each print-row (board-rows board)))

(define (score board)
  (define (row-score row)
    (sequence-fold
     (lambda (acc cell)
       (if (board-cell-is-marked cell)
           acc
           (+ acc (board-cell-number cell))))
     0
     row))
  (sum (map row-score (board-rows board))))
   

(define (read-input)
  (with-input-from-file "input.txt"
    (lambda ()
      (sequence->list (in-lines)))))

(define (input)
  (define (read-all-boards in boards)
    (define (parse-row s)
      (list->vector
       (map
        (lambda (s) (make-board-cell (string->number s)))
        (string-split s))))
    (if (null? in)
        boards
        (let ((rows (map parse-row (take (cdr in) 5))))
          (read-all-boards (drop in 6) (cons (board rows) boards)))))
  (let ((in (read-input)))
    (define numbers
      (map string->number (string-split (car in) ",")))
    (cons numbers (read-all-boards (cdr in) '()))))

(define (find-winning-boards)
  (let ((in (input)))
    (let ((numbers (car in))
          (boards (cdr in)))
      (define (iter numbers boards all-winner-boards)
        (if (null? numbers)
            all-winner-boards
            (let ((number (car numbers)))
              (for-each
               (lambda (board) (mark board number))
                boards)
              (let ((winner-boards (filter is-winning boards)))
                (if (null? winner-boards)
                    (iter (cdr numbers) boards all-winner-boards)
                    (iter
                     (cdr numbers)
                     (filter
                      (lambda (board) (not (is-winning board)))
                      boards)
                     (cons
                      (cons number winner-boards)
                      all-winner-boards)))))))
        (reverse (iter numbers boards '())))))

(define (last-element lst)
  (if (null? (cdr lst))
      (car lst)
      (last-element (cdr lst))))

(let ((winning-boards (find-winning-boards)))
  (display "Task 1: ")
  (display
    (let ((number (car (car winning-boards)))
          (board (cadr (car winning-boards))))
      (* number (score board))))
  (newline)
  (display "Task 2: ")
  (let ((last-winning-board (last-element winning-boards)))
    (display
     (let ((number (car last-winning-board))
           (board (car (cdr last-winning-board)))) 
      (* number (score board)))))
  (newline))