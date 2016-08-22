#lang racket

; A Direction is one of:
;   'up
;   'down
;   'right
;   'left

; A RoomId is a Number
; A Health is a Number
; A State is a Symbol
; An ItemName is a Symbol
; An ActionName is a Symbol

; A Room is (room RoomId String List(Exit) List(Item))
; A Player is (player Symbol RoomId Health List(Item))
; An Exit is (exit Direction RoomId)
; A Monster is (monster RoomId Symbol Health)
; A Transition is (transition State Char State)
; An Item is (item ItemName List(Action))
; An Action is (action ActionName Fun)

(struct room (id description exits items) #:mutable)
(struct player (name room-id health items) #:mutable)
(struct exit (direction room-id))
(struct monster (room-id description health) #:mutable)
(struct transition (source char target))
(struct item (name actions))
(struct action (name handler))

; command-state-machine : List(Transition)

(define command-state-machine
  (list
   (transition 'start #\space 'start)
   
   (transition 'start        #\q     's1)
   (transition 's1           #\u     's2)
   (transition 's2           #\i     's3)
   (transition 's3           #\t     'quit)
   (transition 'quit         #\space 'quit)
   
   (transition 'start        #\m     's4)
   (transition 's4           #\o     's5)
   (transition 's5           #\v     's6)
   (transition 's6           #\e     's7)
   (transition 's7           #\space 's7)
   
   (transition 's7           #\n     's8)
   (transition 's8           #\o     's9)
   (transition 's9           #\r     's10)
   (transition 's10          #\t     's11)
   (transition 's11          #\h     'move-up)
   (transition 'move-up   #\space 'move-up)
   
   (transition 's7           #\s     's12)
   (transition 's12          #\o     's13)
   (transition 's13          #\u     's14)
   (transition 's14          #\t     's15)
   (transition 's15          #\h     'move-down)
   (transition 'move-down   #\space 'move-down)
   
   (transition 's7           #\e     's16)
   (transition 's16          #\a     's17)
   (transition 's17          #\s     's18)
   (transition 's18          #\t     'move-right)
   (transition 'move-right    #\space 'move-right)
   
   (transition 's7           #\w     's19)
   (transition 's19          #\e     's20)
   (transition 's20          #\s     's21)
   (transition 's21          #\t     'move-left)
   (transition 'move-left    #\space 'move-left)
   
   (transition 'start        #\h     's22)
   (transition 's22          #\i     's23)
   (transition 's23          #\t     's24)
   (transition 's24          #\space 's24)
   
   (transition 's24          #\Y     's25)
   (transition 's25          #\e     's26)
   (transition 's26          #\t     's27)
   (transition 's27          #\i     'hit-Hulk)
   (transition 'hit-Hulk     #\space 'hit-Hulk)
   
   (transition 'start        #\g     's28)
   (transition 's28          #\r     's29)
   (transition 's29          #\a     's30)
   (transition 's30          #\b     'grab)
   (transition 'grab         #\space 'grab)
   
   (transition 'start        #\d     's31)
   (transition 's31          #\r     's32)
   (transition 's32          #\o     's33)
   (transition 's33          #\p     'drop)
   (transition 'drop         #\space 'drop)
   
   (transition 'start        #\u     's34)
   (transition 's34          #\s     's35)
   (transition 's35          #\e     'use)
   (transition 'use          #\space 'use)
   ))

(define (zap-wand item)
  (if (chance? 3)
      (zap-hits-self)
      (zap-hits-monsters)))

(define (zap-hits-self)
  (printf "The wand backfires!~%")
  (let ((damage (random 3)))
    (set-player-health! me (- (player-health me) damage))))

(define (zap-hits-monsters)
  (if (null? (co-located-monsters))
      (printf "nothing here to zap at!~%")
      (for/list ((monster (co-located-monsters)))
        (zap-monster monster))))

(define (zap-monster monster)
  (let ((damage (random 5)))
    (if (= damage 0)
        (printf "the wand splutters.~%")
        (begin
          (printf "the wand produces fireballs that hit the ~a~%" (monster-description monster))
          (set-monster-health! monster (- (monster-health monster) damage))
          (when (<= (monster-health monster) 0)
            (set! monsters (remove monster monsters))
            (printf "the ~a dies~%" (monster-description monster)))))))

; wand-actions : List(action)

(define wand-actions
  (list (action 'zap zap-wand)))



; dungeon : List(Room)

(define dungeon 
  (list
   (room 1 "a green room" (list (exit 'up 2) (exit 'right 3)(exit 'left 4))(list (item 'wand wand-actions)))
   (room 2 "a red room" (list (exit 'down 1)(exit 'left 5)(exit 'right 6))'())
   (room 3 "a room that smells of pizza" (list (exit 'left 1)(exit 'up 6))'())
   (room 4 "a glowing room" (list (exit 'right 1)(exit 'up 5))'())
   (room 5 "a room of danger" (list (exit 'right 2)(exit 'down 4))'())
   (room 6 "a chamber" (list (exit 'left 2)(exit 'down 3))'())))

; me : Player(define me (player 'Gangster 1))

(define me (player 'Gangster 1 10 '()))
; monsters : List(Monster)

(define monsters
  (list
   (monster 1 'Hulk 10)(monster 5 'Wolf 10)))

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
    (when (monster-actions monsters)
      (command-loop))))

; monster-actions : List(Monster) -> Boolean

(define (monster-actions monsters)
  (if (null? monsters)
      #t
      (if (monster-action (car monsters))
          (monster-actions (cdr monsters))
          #f)))

; monster-in-same-room? : Monster -> Boolean

(define (monster-in-same-room? monster)
  (= (player-room-id me) (monster-room-id monster)))

; chance : Number -> Boolean

(define (chance? n)
  (= (random n) 0))

; player-dies : -> Boolean
(define (player-dies)
  (printf "You die!~%")
  #f)

; monster-hits : Monster Number -> Boolean
(define (monster-hits monster damage)
  (printf "The ~a hits!~%" (monster-description monster))
  (set-player-health! me (- (player-health me) damage))
  (if (<= (player-health me) 0)
      (player-dies)
      #t))

; monster-misses : Monster -> Boolean

(define (monster-misses monster)
  (printf "The ~a misses!~%" (monster-description monster))
  #t)

; monster-attacks : Monster -> Boolean

(define (monster-attacks monster)
  (let ((damage (random 3)))
    (if (> damage 0)
        (monster-hits monster damage)
        (monster-misses monster))))

; monster-action Monster -> Boolean
; perform an action and return #t when the game can continue
; return #f when the game is over.

(define (monster-action monster)
  (if (and (monster-in-same-room? monster) (chance? 2))
      (monster-attacks monster)
      #t))

(define (say-hello)
  (printf "Welcome to the dungeon ~a~%" (player-name me)))

(define (co-located-monsters)
  (let ((room (get-room (player-room-id me))))
    (filter (λ (m) (= (monster-room-id m) (room-id room))) monsters)))

(define (describe-current-location)
  (let ((room (get-room (player-room-id me))))
    (if (room? room)
        (let ((co-located-monsters
               (filter (λ (m) (= (monster-room-id m) (room-id room))) monsters)))
          (printf "You are in ~a~%" (room-description room))
          (for/list ((monster co-located-monsters))
            (printf "There is a ~a here.~%" (monster-description monster))))
        (printf "I have no idea where you are!~%"))))

(define (perform-player-command)
  (let ((command (read)))
    (cond ((equal? command 'quit) #f)
          ((and (list? command) (eq? (car command) 'move))
           (move-player (cadr command))
           #t)
          ((and (list? command) (eq? (car command) 'hit))
           (player-hits (cadr command)))
          (#t (printf "Huh?~%")
              #t)))) 

; find-monster : Symbol -> Monster or #f
; Returns the monster in the same location as the player and with the 
; supplied description. Returns #f otherwise.

(define (find-monster description)
  (findf (λ (m) (and (monster-in-same-room? m) (eq? (monster-description m) description))) monsters))

(define (monster-dies monster)
  (printf "The ~a dies!~%" (monster-description monster))
  (set! monsters (remove monster monsters)))

(define (hit-monster monster damage)
  (set-monster-health! monster (- (monster-health monster) damage))
  (if (<= (monster-health monster) 0)
      (monster-dies monster)
      (printf "You hit the ~a~%" (monster-description monster))))

(define (player-hits target)
  (let ((monster (find-monster target)))
    (if (monster? monster)
        (let ((damage (random 3)))
          (if (> damage 0)
              (hit-monster monster damage)
              (printf "You miss the ~a~%" target)))
        (printf "There is no ~a here.~%" target))))

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