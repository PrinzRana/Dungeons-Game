#lang racket

(require "client.rkt")
(require "server.rkt")
(require "images.rkt")

(define (run)
  (printf "Type a port number (e.g. 1234): ")
  (let ((port (read)))
    (thread (λ () (test-server port)))
    (test-client port "fred" 'fred)))

(run)