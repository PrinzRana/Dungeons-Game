#lang racket

; A RoomId is a Number
; A Room is (room RoomId String)
; A Player is (player Symbol RoomId)

(struct room (id description))
(struct player (name room-id))

; dungeon : List(Room)

(define dungeon 
  (list
   (room 1 "a green room")
   (room 2 "a red room")
   (room 3 "a room that smells of pizza")
   (room 4 "a glowing room")
   (room 5 "a blue room")))

; me : Player

(define me (player 'Gangster 1))

; Room? is one of:
;   Room
;   #f
; get-room : RoomId -> Room?

(define (get-room id)
  (findf (λ (room) (= (room-id room) id)) dungeon))

; This is the entry point for the game.
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
          (#t (printf "Huh?~%")
              #t))))

(define (say-goodbye)
  (printf "Bye~%" ))