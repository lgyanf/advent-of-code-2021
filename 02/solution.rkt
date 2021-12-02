#lang racket/base

(require racket/generic)
(require racket/sequence)
(require racket/string)

; task 1 solution

(define-generics submarine
  (down submarine value)
  (up submarine value)
  (forward submarine value))
  
(struct simple-submarine (x depth)
  #:methods gen:submarine
  [(define (up sub value)
     (simple-submarine (simple-submarine-x sub) (- (simple-submarine-depth sub) value)))
   (define (down sub value)
     (simple-submarine (simple-submarine-x sub) (+ (simple-submarine-depth sub) value)))
   (define (forward sub value)
     (simple-submarine (+ (simple-submarine-x sub) value) (simple-submarine-depth sub)))])

(struct aimed-submarine (x depth aim)
  #:methods gen:submarine
  [(define (down sub value)
     (aimed-submarine
       (aimed-submarine-x sub)
       (aimed-submarine-depth sub)
       (+ (aimed-submarine-aim sub) value)))
   (define (up sub value)
     (aimed-submarine
       (aimed-submarine-x sub)
       (aimed-submarine-depth sub)
       (- (aimed-submarine-aim sub) value)))
   (define (forward sub value)
     (aimed-submarine
       (+ (aimed-submarine-x sub) value)
       (+ (aimed-submarine-depth sub) (* (aimed-submarine-aim sub) value))
       (aimed-submarine-aim sub)))])

(define (input)
  (define (parse-line line)
    (let ((parts (string-split line " ")))
      (cons (car parts) (string->number (cadr parts)))))
  (with-input-from-file "input.txt"
    (lambda ()
      (map
       parse-line
       (sequence->list (in-lines))))))

(define (move commands initial)
  (sequence-fold
    (lambda (sub cmd)
      (let ((command-name (car cmd))
            (value (cdr cmd)))
       (cond
         ((equal? command-name "forward") (forward sub value))
         ((equal? command-name "down") (down sub value))
         ((equal? command-name "up") (up sub value))
         (else (error "Unknown command" command-name)))))
    initial
    commands))

; task 1 solution output
(define task-1-position (move (input) (simple-submarine 0 0)))
(display "Task 1: ")
(display (* (simple-submarine-x task-1-position) (simple-submarine-depth task-1-position)))
(newline)

; task 2 solution output
(define task-2-position (move (input) (aimed-submarine 0 0 0)))
(display "Task 2: ")
(display (* (aimed-submarine-x task-2-position) (aimed-submarine-depth task-2-position)))
(newline)
