#lang racket

(provide
 state
 transition
 machine
 machine-start
 machine-fire)

(struct state
  (name)
  #:transparent)

(struct transition
  ;;; The guard is a predicate that is supplied with an event
  ;;; and an environment. It returns true when the transition
  ;;; is enabled. The action is an (event,environment) -> environment
  ;;; mapping...
  (source target guard action)
  #:transparent)

(struct machine
  ;;; The name of the starting state, the set of states and the
  ;;; set of transitions...
  (start states transitions)
  #:transparent)

(define (machine-get-enabled machine state-name event-name event-args env)
  ;;; Return a list of the enabled transitions...
  (filter 
   (λ (transition) 
     (and
      (equal? (transition-source transition) state-name)
      (transition-enabled? transition event-name event-args env)))
   (machine-transitions machine)))

(define (machine-fire machine state-name event-name event-args env)
  ;;; Returns two values:
  ;;; (1) The new state name.
  ;;; (2) The new envionrment.
  ;;; If there are multiple enabled transitions then one is
  ;;; selected at random, if no transitions are enabled then
  ;;; the machine stays in the same state...
  (let ((enabled (machine-get-enabled machine state-name event-name event-args env)))
    (if (null? enabled)
        (values state-name env)
        (let ((transition (car enabled)))
          (values
           (transition-target transition)
           ((transition-action transition) event-name event-args env))))))

(define (transition-enabled? transition event-name event-args env)
  ((transition-guard transition) event-name event-args env))