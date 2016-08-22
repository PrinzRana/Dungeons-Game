#lang racket

; A Direction is one of:
;   'up
;   'down
;   'right
;   'left

; A RoomId is a Number

; A Room is (room RoomId String List(Exit))
; A Player is (player Symbol RoomId)
; An Exit is (exit Direction RoomId)

(struct room (id description exits))
(struct player (name room-id) #:mutable)
(struct exit (direction room-id))

; dungeon : List(Room)

(define dungeon 
  (list
   (room 1 "a green room" (list (exit 'up 2) (exit 'right 3)))
   (room 2 "a red room" (list (exit 'down 1)))
   (room 3 "a room that smells of pizza" (list (exit 'left 1) (exit 'down 4)))
   (room 4 "a glowing room" (list (exit 'up 3)))
   (room 5 "a room that smells of pizza" (list (exit 'right 1)))))


; me : Player(define me (player 'Gangster 1))

(define me (player 'Gangster 1))

; Room? is one of:
;   Room
;   #f
; get-room : Number -> Room?

(define (get-room id)
  (findf (λ (room) (= (room-id room) id)) dungeon))

(define (play)
  (say-hello)
  (command-loop)
  (say-goodbye))

; Continue to process commands until the player quits.
(define (command-loop)
  (describe-current-location)
  (when (perform-player-command)
    (command-loop)))

(define (say-hello)
  (printf "Welcome to the dungeon ~a~%" (player-name me)))

(define (describe-current-location)
  (let ((room (get-room (player-room-id me))))
    (if (room? room)
        (printf "You are in ~a~%" (room-description room))
        (printf "I have no idea where you are!~%"))))

(define (perform-player-command)
  (let ((command (read)))
    (cond ((equal? command 'quit) #f)
          ((and (list? command) (eq? (car command) 'move))
           (move-player (cadr command))
           #t)
          (#t (printf "Huh?~%")
              #t))))

(define (move-player direction)
  (let ((room (get-room (player-room-id me))))
    (if (has-exit? room direction)
        (set-player-room-id! me (exit-leads-to room direction))
        (printf "You cannot go ~a from here~%" direction))))

; has-exit : Room Direction -> Boolean
(define (has-exit? room direction)
  (findf (λ (e) (eq? (exit-direction e) direction)) (room-exits room)))

; exit-leads-to : Room Direction -> RoomId
(define (exit-leads-to room direction)
  (exit-room-id (findf (λ (e) (eq? (exit-direction e) direction)) (room-exits room))))

(define (say-goodbye)
  (printf "Bye ~a~%" (player-name me)))