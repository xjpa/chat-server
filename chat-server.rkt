#lang racket

(require racket/tcp)

(define clients '())

(define (broadcast message sender)
  (printf "broadcasting: ~a~n" message)
  (for ([client (in-list clients)])
    (unless (eq? client sender)
      (with-handlers ([exn:fail? (lambda (exn) 
                                   (printf "error sending to a client: ~a~n" (exn-message exn)))])
        (displayln message (cdr client))
        (flush-output (cdr client))))))

(define (handle-client in out)
  (let ([client (cons in out)])
    (set! clients (cons client clients))
    (printf "new client connected. total clients: ~a~n" (length clients))
    (let loop ()
      (with-handlers ([exn:fail? (lambda (exn) 
                                   (printf "client disconnected: ~a~n" (exn-message exn)))])
        (define message (read-line in))
        (printf "received message: ~a~n" message)
        (when (not (eof-object? message))
          (broadcast (format "client says: ~a" message) client)
          (loop))))
    (set! clients (remove client clients))
    (printf "client disconnected. remaining clients: ~a~n" (length clients))))

(define (start-server port)
  (define listener (tcp-listen port 5 #t))
  (printf "chat server is running on port ~a~n" port)
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread (lambda () (handle-client in out)))
    (loop)))

(start-server 8080)
