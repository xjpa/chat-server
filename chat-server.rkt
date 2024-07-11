#lang racket

(require racket/tcp)

(define clients '())
(define client-id-counter 0)

(define (generate-client-id)
  (set! client-id-counter (add1 client-id-counter))
  (format "client-~a" client-id-counter))

(define (broadcast message sender-id)
  (printf "broadcasting: ~a~n" message)
  (for ([client (in-list clients)])
    (unless (equal? (car client) sender-id)
      (with-handlers ([exn:fail? (lambda (exn) 
                                   (printf "error sending to ~a: ~a~n" (car client) (exn-message exn)))])
        (displayln message (caddr client))
        (flush-output (caddr client))))))

(define (handle-client in out)
  (let* ([client-id (generate-client-id)]
         [client (list client-id in out)])
    (set! clients (cons client clients))
    (printf "~a connected. total clients: ~a~n" client-id (length clients))
    (displayln (format "welcome! you are ~a" client-id) out)
    (flush-output out)
    (broadcast (format "~a has joined the chat." client-id) client-id)
    (let loop ()
      (with-handlers ([exn:fail? (lambda (exn) 
                                   (printf "~a disconnected: ~a~n" client-id (exn-message exn)))])
        (define message (read-line in))
        (printf "received from ~a: ~a~n" client-id message)
        (when (not (eof-object? message))
          (broadcast (format "~a says: ~a" client-id message) client-id)
          (loop))))
    (set! clients (remove client clients))
    (broadcast (format "~a has left the chat." client-id) client-id)
    (printf "~a disconnected. remaining clients: ~a~n" client-id (length clients))))

(define (start-server port)
  (define listener (tcp-listen port 5 #t))
  (printf "chat server is running on port ~a~n" port)
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread (lambda () (handle-client in out)))
    (loop)))

(start-server 8080)
