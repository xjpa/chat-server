#lang racket

(define (start-server port)
  (define listener (tcp-listen port))
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread (lambda () (handle-client in out)))
    (loop)))

(define (handle-client in out)
  (let loop ()
    (define message (read-line in))
    (unless (eof-object? message)
      (displayln message)
      (loop))))

(start-server 8080)