#lang racket

(require "parse.rkt")
(require racket/tcp)
(require racket/serialize)
(require racket/match)
(require trace/calltrace-lib)
(require "cell.rkt")
(require "state-machine.rkt")
(require "logging.rkt")

(provide test-server)

;;; How many seconds before action is imposed...

(define turn-duration 3)

(define turns 0)

;;; ***********************************************************************************************
;;; ***************************************  The Dungeon  *****************************************
;;; ***********************************************************************************************

;;; A dungeon is an instance of map* that contains the locations, their contents and the connections
;;; between rooms. The dungeon is serlializable so that the current state can be saved to persistent
;;; storage. Players must be removed before it is saved...

(serializable-struct map* 
  ;;; A map* contains the locations and the starting location.
  ;;; Each location contains all its contained elements. These change as a result of actions.
  (start locations) 
  #:mutable #:transparent)

(serializable-struct location 
  ;;; A location can be referenced by its unique id. It contains a description that is used
  ;;; to inform a player where they are. The exits have directions and lead to other locations.
  ;;; The contents of a room change as a result of actions. Locations are arranged as a grid
  ;;; so tat they can easily be displayed graphically. The (x,y) position is recorded and must 
  ;;; be unique for each level of the dungeon...
  (id x y description exits items monsters players visited runes) 
  #:mutable #:transparent)

(serializable-struct exit 
  ;;; An exit is contained by a location and has a direction. A direction is one of the 
  ;;; compass points represented as a symbol. The exit leads to a location.
  (direction description location) 
  #:mutable #:transparent)

(define (player-id player)
  (user-id (client-user (player-client player))))

(define (player-equal? p1 p2 rec?)
  (eq? (player-id p1) (player-id p2)))

(define (player-hash player rec)
  (eq-hash-code (player-id player)))

(define (player-hash2 player rec)
  (eq-hash-code (player-id player)))

(serializable-struct player 
  ;;; A player is connected to the server via a client structure. The player has a number
  ;;; of health points that can be increased or decreased by various actions. When the health
  ;;; becomes less than 1 the player is dead and removed from the dungeon. The inventory
  ;;; contains the items that the player is currently carrying.
  (client profile inventory) 
  #:methods gen:equal+hash
  [(define equal-proc player-equal?)
   (define hash-proc player-hash)
   (define hash2-proc player-hash2)]
  #:mutable 
  #:transparent)

(serializable-struct item
  ;;; An item is something that can live in a location or be carried by a player.
  ;;; Each item has a type and an identity. The type defines what can be done with
  ;;; the item and what affect it might have on the user...
  (type* id env)
  #:transparent)

(struct item-type
  ;;; Each item type has a name and a collection of handlers. A handler processes a command
  ;;; by parsing it and then, if recognized, passing it and its context on to an action...
  (name constructor handlers)
  #:transparent)

(struct item-type-handler
  ;;; An item type handler includes a parser and an action. The parser processes the raw
  ;;; text and extracts any data that is passed on as an environment together with the player
  ;;; and the item to the action...
  (parser action)
  #:transparent)

(serializable-struct monster
  ;;; A monster has a type that defines how the monster operates and its description.
  ;;; Like a player, the monster has health that must be maintained above 0 for the
  ;;; monster to be active. Each monster runs in its own thread and has a timeout before
  ;;; it grabs the semaphore and performs an action defined by its type.
  (type* id state env health inventory) 
  #:mutable #:transparent)

(struct monster-type
  ;;; A monster type has a unique name. Its speed dictates how fast the monster moves in 
  ;;; seconds. It is constructed using a function that will initialize the env health 
  ;;; and inventory as appropriate.
  (name description speed health recovery-rate constructor printer handler behaviour)
  #:transparent)

(struct event
  ;;; An event is raised when something happens. All players handle events in the same way
  ;;; and monsters handle events on a type-basis.
  (name location args)
  #:transparent)

(define (make-event name location . args)
  (event name location args))

;;; Item operations...

(define item-types '())

(define (define-item-type name constructor . handlers)
  (set! item-types (cons (item-type name constructor handlers) item-types)))

(define (get-item-type name)
  (if (has-item-type? name)
      (car (memf (λ (item-type) (eq? (item-type-name item-type) name)) item-types))
      (raise-user-error 'item-type "missing ~a" name)))

(define (has-item-type? name)
  (memf (λ (item-type) (eq? (item-type-name item-type) name)) item-types))

(define-item-type 'cheese
  (λ () (item 'cheese (new-item-identifier) '((energy . 10) (weight . 5))))
  (item-type-handler 
   (seq (literal "eat") (var 'id))
   (λ (env item player fail)
     (if (equal? (lookup 'id env) (symbol->string (item-id item)))
         (if (is-edible? item)
             (player-eats player item)
             (player-format player "Eating ~a would give you indigestion." (lookup 'id env)))
         (fail)))))

(define-item-type 'programming-text-book
  (λ () (item 'programming-text-book (new-item-identifier) '((energy . 10) (weight . 5))))
  (item-type-handler 
   (seq (literal "read") (var 'id))
   (λ (env item player fail)
     (if (equal? (lookup 'id env) (symbol->string (item-id item)))
         (if (is-readable? item)
             (player-reads player item)
             (player-format player "Reading ~a makes you look like a fool." (lookup 'id env)))
         (fail)))))

(define-item-type 'gold
  (λ () (item 'gold (new-item-identifier) '((weight . 5)))))

(define-item-type 'drracket
  (λ () (item 'drracket (new-item-identifier) '((damage . 10) (weight . 5))))
  (item-type-handler
   (seq (literal "wield") (var 'id))
   (λ (env item player fail)
     (if (equal? (lookup 'id env) (symbol->string (item-id item)))
         (player-wield player item)
         (fail))))
  (item-type-handler
   (seq (literal "unwield") (var 'id))
   (λ (env item player fail)
     (if (equal? (lookup 'id env) (symbol->string (item-id item)))
         (player-unwield player item)
         (fail)))))

(define-item-type 'lambda
  (λ () (item 'lambda (new-item-identifier) '((damage . 10) (weight . 5))))
  (item-type-handler
   (seq (literal "wield") (var 'id))
   (λ (env item player fail)
     (if (equal? (lookup 'id env) (symbol->string (item-id item)))
         (player-wield player item)
         (fail))))
  (item-type-handler
   (seq (literal "unwield") (var 'id))
   (λ (env item player fail)
     (if (equal? (lookup 'id env) (symbol->string (item-id item)))
         (player-unwield player item)
         (fail)))))

(define-item-type 'armour 
  (λ () (item 'armour (new-item-identifier) '((protection . 10) (weight . 5))))
  (item-type-handler
   (seq (literal "wear") (var 'id))
   (λ (env item player fail)
     (if (equal? (lookup 'id env) (symbol->string (item-id item)))
         (player-wear player item)
         (fail))))
  (item-type-handler
   (seq (literal "remove") (var 'id))
   (λ (env item player fail)
     (if (equal? (lookup 'id env) (symbol->string (item-id item)))
         (player-remove player item)
         (fail)))))

(define (is-edible? item)
  (match (item-type* item)
    ('cheese #t)
    (else #f)))

(define (is-readable? item)
  (match (item-type* item)
    ('programming-text-book #t)
    (else #f)))

(define (lookup id env)
  (cdr (assoc id env)))

;;; Monster operations...

(define monster-types '())

(define (define-monster-type name description speed health recovery-rate behaviour constructor printer handler)
  ;;; Called in order to define a new monster type. Once defined
  ;; the new type can be used to create multiple instances. Each
  ;; instance will have a unique identifier...
  (set! monster-types 
        (cons (monster-type 
               name 
               description
               speed 
               health 
               recovery-rate 
               constructor 
               printer 
               handler 
               behaviour)
              monster-types)))

(define (has-monster-type? name)
  (memf (λ (m) (eq? (monster-type-name m) name)) monster-types))

(define (get-monster-type name)
  (if (has-monster-type? name)
      (car (memf (λ (m) (eq? (monster-type-name m) name)) monster-types))
      (raise-user-error 'monster-type "missing ~a" name)))

(define (monster-location monster)
  (if (dungeon-contains-monster? monster)
      (car (memf (λ (l) (memf (λ (m) (equal? m monster)) (location-monsters l))) (map*-locations dungeon)))
      #f))

;;; Definition of the monster types that are available. NB the client
;;; tool should have an image for each of these types so that it knows
;;; how to display it to the player...

(define general-monster-description "An unkown monster")

(define (monster-flight-behaviour monster)
  (let ((location (monster-location monster)))
    (if (and location (location-has-exits? location) (chance? 4))
        (let* ((exits (location-exits location))
               (exit (choose exits)))
          (dungeon-move-monster monster location exit)
          #t)
        #f)))

(define (monster-fight-behaviour monster)
  (let ((location (monster-location monster)))
    (if (and location (not (null? (location-players location))) (chance? 2))
        (let* ((player (choose (location-players location)))
               (damage (random 3))
               (event (make-event 'monster-hits-player location (monster-id monster) (player-id player) damage)))
          (raise-event event)
          #t)
        #f)))

(define (combine-monster-behaviours . behaviours)
  (λ (monster)
    (define (try-behaviours behaviours)
      (if (not (null? behaviours))
          (or ((car behaviours) monster)
              (try-behaviours (cdr behaviours)))
          #f))
    (try-behaviours behaviours)))

(define-monster-type 'empty-set 
  "An empty contains no elements. It's cardinality is 0."
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "an empty set"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define-monster-type 'state-machine 
  "A state machine consists of states and transitions.
   Both states and transitions have labels. The labels
   on transitions are members of an alphabet and a path
   through the machine describes a legal word in the
   language recognized by the machine."
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "a state machine"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define-monster-type 'arduino 
  "An arduino is a single-board micro-controller." 
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "an arduino"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define-monster-type 'flip-flop 
  "A flip-flop is a circuit with two stable states.
   It can be used to store state information." 
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "a flip-flop"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define-monster-type 'tree 
  "A tree is a data structure that has nodes and edges. It has a root
   node, internal nodes and leaf nodes. Nodes have links to 0 or more
   child nodes. Tree nodes and edges can be labelled with information." 
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "a tree"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define-monster-type 'graph 
  "A graph consists of nodes and edges between nodes. Unlike a tree a
   basic graph may contain cycles so that following edges between nodes
   can lead back to the same node. Nodes and edges can be labelled with
   information." 
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "a graph"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define-monster-type 'proper-list 
  "A proper list is either () (nil) or consists of cons-cells each with a head (car) 
   and tail (cdr). A list is proper when the tail of each cell contains a proper list. 
   The head of each cell contains any information." 
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "a proper list"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define-monster-type 'venn-diagram 
  "A Venn Diagram shows sets as circles where the inside of the circle denotes the
   elements of the set. Where the circles intersect represents those elements that
   are in both sets." 
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "a Venn Diagram"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define-monster-type 'hash-table 
  "A hash table is used to associate keys with data values. The table uses a hashing
   function to map a key to an index into a vector of buckets. A bucket is a list that
   contains cons-pairs associating keys with values. The hashing function makes lookup
   and update of the table much quicker that just using a linked list of cons pairs." 
  2 10 3
  (machine
   's
   (list (state 's))
   (list (transition 's 's (λ (event args env) #t) (λ (event args env) env))))
  (λ (monster) monster)
  (λ (monster) (format "a hash table"))
  (combine-monster-behaviours
   monster-flight-behaviour
   monster-fight-behaviour))

(define ideal-profile
  ; Each time a player kills a topic they acquire knowledge about that
  ; topic. After n (where n is randomly set at the start of the game)
  ; kills for a given topic, the player masters the topic. This provides
  ; the game with a goal: to master all the topics.
  (map (λ (monster-type) (cons (monster-type-name monster-type) (+ (random 5) 3))) monster-types))

(define (new-monster type-name id . args)
  ;;; Used to create a specific instance of a monster type...
  (let* ((type (get-monster-type type-name))
         (behaviour (monster-type-behaviour type))
         (start (machine-start behaviour))
         (monster (monster type-name id start '() (monster-type-health type) '())))
    (apply (monster-type-constructor type) (cons monster args))))

;;; Direction related actions...

(define (direction? d)
  ;;; Directions are represented by symbols named by the compass points and up, down...
  (member d '(up down right left up down)))

(define (direction-opposite dir)
  ;;; Return the opposite direction...
  (match dir
    ['up 'down]
    ['down 'up]
    ['right 'left]
    ['left 'right]
    ['up 'down]
    ['down 'up]
    [_ (raise-user-error 'unknown-direction "~a" dir)]))

;;; Dungeon related actions....

(define all-runes
  (list
   (runes 'EXP '(car '(1 2 3 4 5)) '(car ???) 1)
   (runes 'EXP '(cdr '(1 2 3 4 5)) '(cdr ???) '(2 3 4 5))
   (runes 'FUN
          '(define (double x) (+ x x))
          '(define (double x) (+ ??? ???))
          '((double 10) . 20))
   (runes 'FUN 
          '(define (member-of-list x l) (if (null? l) #f (or (eq? (car l) x) (member-of-list x (cdr l)))))
          '(define (member-of-list x l) (if ??? #f (or (??? (car l) x) (member-of-list x (cdr ???)))))
          '((member-of-list 1 '(3 2 1)) . #t))))

(define (choose-runes)
  (if (chance? 3)
      #f
      (choose all-runes)))

(define (dungeon-add-location! id x y description)
  ;;; Create a new empty location. Access to the location is provided by the id...
  (let ((location (location id x y description '() '() '() '() #f (choose-runes))))
    (set-map*-locations! dungeon (cons location (map*-locations dungeon)))
    location))

(define (dungeon-add-monster! id monster)
  (let ((location (dungeon-get-location id)))
    (location-add-monster! location monster)))

(define (dungeon-add-player! player)
  ;;; Place the player in the starting location and tell them where they are...
  (if (location? (map*-start dungeon))
      (if (not (dungeon-contains-player? player))
          (location-add-player! (map*-start dungeon) player 'up)
          (raise-user-error 'already-placed "~a" player))
      (raise-user-error "dungeon incomplete: no start")))

(define (dungeon-add-item! item location-id)
  (let ((location (dungeon-get-location location-id)))
    (set-location-items! location (cons item (location-items location)))))

(define (dungeon-connect-one-way! source-id dir description target-id)
  ;;; Modify the exit of the source location to point to the target location.
  ;;; In most cases use location-connect! so that the inverse connection is 
  ;;; made...
  (let ((source (dungeon-get-location source-id))
        (target (dungeon-get-location target-id)))
    (if (location-has-exit? source dir)
        (raise-user-error 'duplicate-exit "~a already has an exit for ~a" source-id dir)
        (if (direction? dir)
            (set-location-exits! source (cons (exit dir description target) (location-exits source)))
            (raise-user-error 'illegal-direction "~a" dir)))))

(define (dungeon-contains-monster? monster)
  (memf (λ (l) (memf (λ (m) (equal? m monster)) (location-monsters l))) (map*-locations dungeon)))

(define (dungeon-contains-player? player)
  (memf (λ (l) (memf (λ (p) (equal? (player-id p) (player-id player))) (location-players l))) (map*-locations dungeon)))

(define (dungeon-delete-all-players!)
  (log "[DELETE-ALL-PLAYERS]~%")
  (for ((location (dungeon-locations)))
    (set-location-players! location '())))

(define (dungeon-delete-all-monsters!)
  (log "DELETE-ALL-MONSTERS]~%")
  (for ((location (dungeon-locations)))
    (set-location-monsters! location '())))

(define (dungeon-get-location id)
  (if (dungeon-has-location? id)
      (car (memf (λ (l) (eq? (location-id l) id)) (map*-locations dungeon)))
      (raise-user-error 'location-missing "~a" id)))

(define (dungeon-get-identified id)
  (if (dungeon-has-player? id)
      (dungeon-get-player id)
      (if (dungeon-has-monster? id)
          (dungeon-get-monster id)
          #f)))

(define (dungeon-has-location? id)
  (memf (λ (l) (eq? (location-id l) id)) (map*-locations dungeon)))

(define (dungeon-get-player id)
  (findf (λ (player) (equal? (player-id player) id)) (dungeon-players)))

(define (dungeon-has-player? id)
  (not (eq? (dungeon-get-player id) #f)))

(define (dungeon-kill-all-monsters!)
  (log "[KILL-ALL-MONSTERS]~%")
  (for ((monster (dungeon-monsters)))
    (set-monster-health! monster -1)))

(define (dungeon-locations)
  (map*-locations dungeon))

(define (dungeon-players)
  (apply append (map location-players (map*-locations dungeon))))

(define (dungeon-get-monster id)
  (findf (λ (monster) (equal? (monster-id monster) id)) (dungeon-monsters)))

(define (dungeon-has-monster? id)
  (not (eq? (dungeon-get-monster id) #f)))

(define (dungeon-has-location-at? x y)
  (memf (λ (location) (and (= (location-x location) x) (= (location-y location) y))) (map*-locations dungeon)))

(define (dungeon-monsters)
  (apply append (map location-monsters (map*-locations dungeon))))

(define (dungeon-move player dir)
  ;;; Try to move the player from their current location in the supplied
  ;;; direction. This can fail if the direction has no exit. In either case
  ;;; inform the player of the outcome...
  (let ((location (player-location player)))
    (if (location-has-exit? location dir)
        (let ((target-location (exit-location (location-get-exit location dir))))
          (location-entered target-location)
          (let* ((source-id (location-id location))
                 (target-id (location-id target-location))
                 (target-description (location-description target-location))
                 (exit-directions (map (λ (exit) (exit-direction exit)) (location-exits target-location))))
            (log "[MOVE-PLAYER] ~a ~a~%" (player-id player) dir)
            (player-message player (player-moves source-id target-id dir target-description exit-directions))
            (location-delete-player! location player dir)
            (location-add-player! target-location player dir)
            (player-describe-room! player)))
        (player-format player "no exit ~a" dir))))

(define (dungeon-move-monster monster location exit)
  (log "[MOVE-MONSTER] ~a ~a~%" (monster-id monster) (exit-direction exit))
  (location-delete-monster! location monster)
  (location-inform-players-monster-leaves location monster exit)
  (location-add-monster! (exit-location exit) monster)
  (location-inform-players-new-monster (exit-location exit) monster exit))

(define (dungeon-start! id description)
  ;;; Create the first location. When new players join they will be placed in 
  ;;; the starting location...
  (set-map*-start! dungeon (dungeon-add-location! id 0 0 description)))

(define (dungeon-start-monsters)
  ;;; At the start of time each monster in the dungeon will start to behave
  ;;; automomously...
  (for ((location (map*-locations dungeon)))
    (for ((monster (location-monsters location)))
      (monster-start monster))))

(define (dungeon-start-turns)
  ;;; A turn is a number of seconds. Each turn players are reviewed in terms of
  ;;; resource usage and recovery. The thread dies when there are no players left...
  (define (turn)
    (when (>= turns 0)
      (sleep turn-duration)
      (semaphore-wait semaphore)
      (log "[TURN]~%")
      (set! turns (+ turns 1))
      (dungeon-players-turn)
      (semaphore-post semaphore)
      (turn)))
  (thread turn))

(define (dungeon-stop-turns)
  (set! turns -999))

(define (dungeon-players-turn)
  ;;; Run through the players and update their profiles...
  (for ((player (dungeon-players)))
    (player-turn player)))

;;; Exit operations...

(define (exit-describe exit)
  (format "~a to the ~a" (exit-description exit) (exit-direction exit)))

;;; Location related actions...

(define (location-add-player! location player dir)
  ;;; Update the state of the location and send out events to all potentially
  ;;; interested parties...
  (log "[LOCATION-ADD-PLAYER] ~a ~a ~a~%" (location-id location) (player-id player) dir)
  (set-location-players! location (cons player (location-players location)))
  (location-entered location)
  (let ((event (make-event 'player-enters-location location player dir)))
    (raise-event event)))

(define (location-add-item! location item)
  (log "[LOCATION-ADD-ITEM] ~a ~a~%" (location-id location) (item-id item))
  (set-location-items! location (cons item (location-items location))))

(define (location-add-monster! location monster)
  (log "[LOCATION-ADD-MONSTER] ~a ~a~%" (location-id location) (monster-id monster))
  (set-location-monsters! location (cons monster (location-monsters location))))

(define (location-connect! source-id dir description target-id)
  (dungeon-connect-one-way! source-id dir description target-id)
  (dungeon-connect-one-way! target-id (direction-opposite dir) description source-id))

(define (location-delete-monster! location monster)
  (log "[LOCATION-DELETE-MONSTER] ~a ~a~%" (location-id location) (monster-id monster))
  (set-location-monsters! location (remove monster (location-monsters location))))

(define (location-delete-player! location player dir)
  ;;; Update the state of the location and send out events to all potentially
  ;;; interested parties...
  (log "[LOCATION-DELETE-PLAYER] ~a ~a ~a~%" (location-id location) (player-id player) dir)
  (set-location-players! location (remove player (location-players location)))
  (let ((event (make-event 'player-leaves-location location player location dir)))
    (raise-event event)))

(define (location-describe-exits location)
  ;;; Return a string describing the exits from this location...
  (define (append-descriptions d1 d2)
    (if (equal? d1 "")
        d2
        (if (equal? d2 "")
            d1
            (format "~a, and ~a" d1 d2))))
  (if (null? (location-exits location))
      "Strangely there are no exits from this room"
      (string-append "There is " (foldr append-descriptions "" (map exit-describe (location-exits location))))))

(define (location-describe-monsters location)
  ;;; Return a string describing the monsters in this location...
  (define (append-descriptions d1 d2)
    (if (equal? d1 "")
        d2
        (if (equal? d2 "")
            d1
            (string-append d1 ", " d2))))
  (if (null? (location-monsters location))
      "There are currently no monsters in this room."
      (string-append
       "The room contains "
       (foldr append-descriptions "" (map monster-description (location-monsters location)))
       ".")))

(define location-counter 0)

(define (new-location-identifier)
  (set! location-counter (+ location-counter 1))
  (string->symbol (format "room~a" location-counter)))

(define item-counter 0)

(define (new-item-identifier)
  (set! item-counter (add1 item-counter))
  (string->symbol (format "i~a" item-counter)))

(define monster-counter 0)

(define (new-monster-identifier)
  (set! monster-counter (add1 monster-counter))
  (string->symbol (string-append "m" (number->string monster-counter))))

(define (location-entered location)
  ;;; When a location is entered (by anything) for the first time, this operation
  ;;; should be called before any other activity in order to dynamically expand 
  ;;; the dungeon.
  (define (create-random-monster)
    (let* ((type (choose monster-types))
           (id (new-monster-identifier)))
      (new-monster (monster-type-name type) id)))
  (define (create-random-item)
    (let ((type (choose item-types)))
      ((item-type-constructor type))))
  (when (not (location-visited location))
    ;;; Record that something has been here...
    (set-location-visited! location #t)
    ;;; Extend the exits and ensure that there is a way out...
    (define (add-monsters limit)
      (when (> limit 0)
        (let ((m (create-random-monster)))
          (location-add-monster! location m)
          (monster-start m)
          (add-monsters (sub1 limit)))))
    (define (add-items limit)
      (when (> limit 0)
        (let ((i (create-random-item)))
          (location-add-item! location i)
          (add-items (sub1 limit)))))
    (define (add-exit dir)
      (let* ((x (location-x location))
             (y (location-y location))
             (new-x (if (eq? dir 'right) (+ x 1) (if (eq? dir 'left) (- x 1) x)))
             (new-y (if (eq? dir 'up) (- y 1) (if (eq? dir 'down) (+ y 1) y))))
        (when (and (chance? 2) (not (location-has-exit? location dir)) (not (dungeon-has-location-at? new-x new-y)))
          (let ((id (new-location-identifier)))
            (dungeon-add-location! id new-x new-y (generate-location-description))
            (location-connect! (location-id location) dir (generate-exit-description) id)))))
    (define (boxed-in? location)
      (let* ((x (location-x location))
             (y (location-y location)))
        (and 
         (dungeon-has-location-at? (+ x 1) y)
         (dungeon-has-location-at? (+ x 1) (+ y 1))
         (dungeon-has-location-at? (+ x 1) (- y 1))
         (dungeon-has-location-at? x (+ y 1))
         (dungeon-has-location-at? x (- y 1))
         (dungeon-has-location-at? (- x 1) y)
         (dungeon-has-location-at? (- x 1) (+ y 1))
         (dungeon-has-location-at? (- x 1) (- y 1)))))
    (define (add-exits)
      (add-exit 'up)
      (add-exit 'down)
      (add-exit 'right)
      (add-exit 'left)
      (when (< (length (location-exits location)) 2)
        (add-exits)))
    (when (not (boxed-in? location))
      (add-exits))
    (add-monsters (random 2))
    (add-items (random 2))))

(define (generate-exit-description)
  (format "~a ~a" (exit-modifier) (exit-type)))

(define (exit-modifier)
  (choose exit-modifiers))

(define exit-modifiers
  '("a low"
    "a high"
    "a black"
    "a green"
    "a yellow"
    "a badly constructed"
    "a red"
    "a nasty looking"
    "a sparkly"
    "an enticing"
    "a dark"
    "a light"
    "a mysterious"
    "an ancient"))

(define (exit-type)
  (choose exit-types))

(define exit-types
  '("door"
    "tunnel"
    "opening"
    "gateway"
    "cat-flap"
    "passage"
    "muddy track"
    "brick road"
    "archway"
    "turnstyle"
    "gap"
    "pair of double-doors"))

(define (generate-location-description)
  (format "~a ~a~a~a" (location-modifier) (location-type) (optional-location-info) (optional-ambience)))

(define (location-modifier)
  (choose location-modifiers))

(define location-modifiers
  '("a dank"
    "a dark"
    "a bright"
    "a green"
    "a smelly"
    "a sinister"
    "a windy"
    "a hushed"
    "a dusty"
    "a badly decorated"
    "a tasteless"
    "an enormous"
    "a small"
    "a cluttered"))

(define (location-type)
  (choose location-types))

(define location-types
  '("cave"
    "room"
    "cavern"
    "hallway"
    "tunnel"
    "wide fireplace"
    "wooden hut"
    "tin shed"
    "cathedral"
    "abandoned warehouse"
    "crumbling mansion"
    "store-cupboard"
    "shack"
    "animal shelter"
    "student pad"
    "lecturer's office"
    "tardis"
    "animal nest"))

(define (optional-location-info)
  (if (chance? 3)
      (choose location-info)
      ""))

(define location-info
  '(" with ooze dripping from the walls"
    " with a nasty stain to one side"
    ". You see evidence of a fight"
    ". Someone has been eating in here"
    ". Something has been eaten in here"
    " with an unpleasant smell"
    " that has some writing on the floor"
    " that has not been tidied for years"
    " that was last decorated in the 1970s"
    " littered with computer magazines"
    " whose owner seems to eat nothing but peanuts"
    " with a granite floor"
    " with a floor that needs sweeping"
    " whose owner lives on doughnuts (Mmmmm doughnuts!)"
    " with magical writing on the walls"
    " that smells of blue cheese"
    " that needs some serious cleaning"))

(define (optional-ambience)
  (if (chance? 3)
      (choose ambience)
      ""))

(define ambience
  '(". Someone is playing a banjo (badly) nearby"
    ". The smell of waffles floats by"
    ". Can you smell waffles in here?"
    ". I'm sure I can smell burgers"
    ". Looking up you see some pretty lights"
    ". Looking down you see some ugly mice"
    ". Someone is singing nearby."
    ". You hear a flushing sound"
    ". Water flows nearby"
    ". A lecturer mutters nearby"
    ". Looking up you see some rude baboons"
    ". A herd of what sound (and smell) like wildebright pass nearby"
    ))

(define (location-has-exit? location dir)
  (memf (λ (exit) (eq? (exit-direction exit) dir)) (location-exits location)))

(define (location-has-exits? location)
  (not (null? (location-exits location))))

(define (location-get-exit location dir)
  (if (location-has-exit? location dir)
      (car (memf (λ (exit) (eq? (exit-direction exit) dir)) (location-exits location)))
      (raise-user-error 'no-exit "~a in ~a" dir location)))

(define (location-get-item location id)
  (car (memf (λ (item) (eq? (item-id item) id)) (location-items location))))

(define (location-has-item? location id)
  (memf (λ (item) (eq? (item-id item) id)) (location-items location)))

(define (location-inform-players-monster-leaves location monster exit)
  ;;; The monster is leaving, raise the events
  (let ((event (make-event 'monster-leaves-room location monster exit)))
    (raise-event event)))

(define (location-inform-players-new-monster location monster exit)
  ;;; The monster is leaving, raise the events
  (let ((event (make-event 'monster-enters-room location monster exit)))
    (raise-event event)))

(define (same-location? l1 l2)
  (equal? (location-id l1) (location-id l2)))

(define (location-next-to? l1 l2)
  (ormap (λ (exit) (same-location? (exit-location exit) l2)) (location-exits l1)))

;;; Monster operations...

(define (monster-alive? monster)
  (> (monster-health monster) 0))

(define (monster-description monster)
  (let ((type (get-monster-type (monster-type* monster))))
    ((monster-type-printer type) monster)))

(define (monster-killed monster)
  ;;; The end of the line for the monster. Inform all clients that the monster
  ;;; is dead...
  (log "[MONSTER-KILLED] ~a~%" (monster-id monster))
  (let* ((location (monster-location monster))
         (event (make-event 'monster-dies location (monster-id monster))))
    (raise-event event)
    (set-location-monsters! location (remove monster (location-monsters location)))))

(define (handle-monster-event monster event)
  ;;; Some events will be handled the same for all monsters and others will be
  ;;; handled in a monster-specific way. The following handles the events that
  ;;; are the same for all monsters...
  (log "[MONSTER-EVENT] ~a ~a ~a~%" (monster-id monster) (event-name event) (location-id (event-location event)))
  (when (monster-alive? monster)
    (match (event-name event)
      ('player-hits-monster 
       (let ((source-id (car (event-args event)))
             (target-id (cadr (event-args event)))
             (damage (caddr (event-args event))))
         (when (equal? target-id (monster-id monster))
           (handle-monster-is-hit monster damage))))
      (else '()))
    ;;; The event is then supplied to the behaviour part of the monster
    ;;; so that it can be handled in a type specific way...
    (let* ((state (monster-state monster))
           (env (monster-env monster))
           (type (get-monster-type (monster-type* monster)))
           (behaviour (monster-type-behaviour type))
           (event-name (event-name event))
           (event-args (event-args event)))
      (let-values
          (((new-state new-env) (machine-fire behaviour state event-name event-args env)))
        (set-monster-state! monster new-state)
        (set-monster-env! monster new-env)))))

(define (handle-monster-is-hit monster damage)
  (log "[MONSTER-HIT] ~a ~a~%" (monster-id monster) damage)
  (set-monster-health! monster (- (monster-health monster) damage)))

(define (monster-start monster)
  ;;; A monster runs in its own thread. The monster is autonomous, it cycles through a
  ;;; loop that first waits for several second depending on the speed of the monster
  ;;; type and then grabs the semaphore in order to get exclusive access to the dungeon
  ;;; state. The action depends on the type of the monster and its current state. After
  ;;; the action, the monster releases the dungeon state...
  (define (monster-loop)
    (let ((type (get-monster-type (monster-type* monster))))
      (sleep (monster-type-speed type))
      (semaphore-wait semaphore)
      (if (> (monster-health monster) 0)
          (begin
            ((monster-type-handler type) monster)
            (semaphore-post semaphore)
            (monster-loop))
          (begin
            (monster-killed monster)
            (log "[STOP-MONSTER] ~a~%" (monster-id monster))
            (semaphore-post semaphore)))))
  (thread
   (λ ()
     (with-handlers
         ([(λ (x) #t) 
           (λ (x) (log "[ERROR] ~a~%" x) (raise x))])
       (log "[START-MONSTER] ~a~%" (monster-id monster))
       (monster-loop)))))

;;; Player actions...

(define (player-command player command)
  (semaphore-wait semaphore)
  (player-handle-command player command)
  (semaphore-post semaphore))

(define (player-eats player item)
  ;;; The item should be edible and should contain an environment with
  ;;; a value for energy. The item is assumed to be acarried by the
  ;;; player. The energy is added to the player's current attribute and
  ;;; the item is removed from the player's inventory...
  (let* ((profile (player-profile player))
         (location (player-location player))
         (energy (lookup 'energy (item-env item)))
         (event (make-event 'player-eats location player item)))
    (set-player-inventory! player (remove item (player-inventory player)))
    (set-profile-energy! profile (min (profile-max-energy profile) (+ (profile-energy profile) energy)))
    (player-format player "You eat the ~a and you feel more energetic." (item-type* item))
    (player-message player (update-profile profile))
    (player-message player (lose-item (item-id item)))
    (raise-event event)))

(define (player-reads player item)
  ;;; The item should be readable and should contain an environment with
  ;;; a value for energy. The item is assumed to be carried by the
  ;;; player. The energy is added to the player's current attribute and
  ;;; the item is removed from the player's inventory...
  (let* ((profile (player-profile player))
         (location (player-location player))
         (energy (lookup 'energy (item-env item)))
         (event (make-event 'player-eats location player item)))
    (set-player-inventory! player (remove item (player-inventory player)))
    (set-profile-energy! profile (min (profile-max-energy profile) (+ (profile-energy profile) energy)))
    (player-format player "You read the ~a and you feel more enlightened." (item-type* item))
    (player-message player (update-profile profile))
    (player-message player (lose-item (item-id item)))
    (raise-event event)))

(define (player-wear player item)
  ;;; The item is some armour that will reduce the damage when the player is hit...
  (let* ((profile (player-profile player))
         (location (player-location player))
         (id (item-id item))
         (wearing (profile-wearing profile))
         (event (make-event 'player-wears location player item)))
    (set-profile-wearing! profile (cons id wearing))
    (player-format player "You wear ~a." id)
    (player-message player (update-profile profile))
    (raise-event event)))

(define (player-wield player item)
  ;;; The item is a weapon and will boost the damage done when the player
  ;;; attacks.
  (let* ((profile (player-profile player))
         (location (player-location player))
         (id (item-id item))
         (event (make-event 'player-wields location player item)))
    (when (profile-wielding profile)
      ;;; Already wielding, let's get rid of it...
      (player-format player "You stop wielding ~a" (profile-wielding profile)))
    (set-profile-wielding! profile id)
    (player-format player "You are using ~a as a weapon." id)
    (player-message player (update-profile profile))
    (raise-event event)))

(define (player-remove player item)
  ;;; Take some armour off...
  (let* ((profile (player-profile player))
         (location (player-location player))
         (id (item-id item))
         (wearing (profile-wearing profile))
         (event (make-event 'player-unwields location player item)))
    (if (member id wearing)
        (begin
          (set-profile-wearing! profile (remove id wearing))
          (player-message player (update-profile profile))
          (player-format player "You remove ~a" id)
          (raise-event event))
        (player-format player "You are not wearing ~s." id))))

(define (player-unwield player item)
  ;;; Stop using this as a weapon...
  (let* ((profile (player-profile player))
         (location (player-location player))
         (event (make-event 'player-unwields location player item)))
    (set-profile-wielding! profile #f)
    (player-message player (update-profile profile))
    (player-format player "You stop using ~a as a weapon" (item-id item))
    (raise-event event)))

(define (player-chat player chat)
  ;;; Chatting is like private messaging between players.
  (let ((id (chat-id chat))
        (message (chat-message chat))
        (event (make-event 'player-chat (player-location player) chat)))
    (if (eq? id (player-id player))
        (player-info player "Chatting to yourself is a sign of madness.")
        (if (dungeon-has-player? id)
            (let* ((target (dungeon-get-player id))
                   (location1 (player-location player))
                   (location2 (player-location target)))
              (if (not (eq? (location-id location1) (location-id location2)))
                  (player-info player "Shouting down corridors just attracts nasty creatures. You must be co-located to chat.")
                  (begin
                    (player-format target "~a whispers to you: ~a" id message)
                    (raise-event event))))
            (player-format player "You look pretty stupid muttering into thin air.")))))

(define (player-check-energy player)
  ;;; If a players energy drops below 1 then they die...
  (let ((profile (player-profile player)))
    (when (<= (profile-energy profile) 0)
      ;;; The die message causes the client to shut down. The 
      ;;; message includes a reason for death...
      (player-info player "You run out of energy.")
      (player-quits player))))

(define (player-handle-command player command)
  ;;; Commands are strings that are processed by behaviour definitions.
  ;;; The behaviour definition provides a parser for the string. If the
  ;;; parse succeeds then an environment of bindings will have been created
  ;;; that is supplied to an action procedure...
  (log "[PLAYER-COMMAND] ~a ~a~%" (player-id player) command)
  (let try-behaviours ((bs behaviours))
    (if (null? bs)
        ;;; Not a native player command, however it might be a command
        ;;; that is handled by something that the player is carrying...
        (player-item-command player command)
        (let ((parser (behaviour-parser (car bs))))
          (parser command 0 '()  
                  (λ (env i) ((behaviour-handler (car bs)) player env)) 
                  (λ () (try-behaviours (cdr bs))))))))

(define (player-has-item? player id)
  (memf (λ (item) (eq? (item-id item) id)) (player-inventory player)))

(define (player-get-item player id)
  (car (memf (λ (item) (eq? (item-id item) id)) (player-inventory player))))

(define (player-item-command player command)
  (let try-items ((items (player-inventory player)))
    (if (null? items)
        (player-info player "huh?")
        (let* ((item (car items))
               (type-name (item-type* item))
               (type (get-item-type type-name)))
          (let try-handlers ((handlers (item-type-handlers type)))
            (if (null? handlers)
                (try-items (cdr items))
                (let ((parser (item-type-handler-parser (car handlers))))
                  (parser command 0 '() 
                          (λ (env index) ((item-type-handler-action (car handlers)) env item player (λ () (try-items (cdr items))))) 
                          (λ () (try-handlers (cdr handlers)))))))))))

(define (player-location player)
  (if (dungeon-contains-player? player)
      (car (memf (λ (l) (memf (λ (p) (equal? (player-id p) (player-id player))) (location-players l))) (map*-locations dungeon)))
      (raise-user-error 'player-not-in-dungeon "~a" player)))

(define (player-describe-room! player)
  (let* ((location (player-location player))
         (client (player-client player))
         (user (client-user client))
         (id (user-id user))
         (description (location-description location))
         (runes (location-runes location))
         (prefix "You are in ")
         (text (string-append prefix description))
         (exits (location-describe-exits location))
         (rune-info (if runes " There are runes written on a wall nearby." ""))
         (text (string-append text " " exits "." rune-info)))
    (client-message! client (clear-monsters))
    (client-message! client (clear-items))
    (client-message! client (room-info text))
    (client-message! client (room-runes runes))
    (for ((other-player (location-players location)))
      (let* ((other-client (player-client other-player))
             (other-user (client-user other-client))
             (other-id (user-id other-user))
             (other-name (user-name other-user)))
        (when (not (eq? id other-id))
          (client-message! client (player-in-room other-id other-name)))))
    (for ((monster (location-monsters location)))
      (client-message! client (monster-in-room (monster-id monster) (monster-type* monster))))
    (for ((item (location-items location)))
      (client-message! client (item-in-room (item-id item) (item-type* item))))))

(define (player-attacks player id)
  (let ((location (player-location player))
        (target (dungeon-get-identified id)))
    (cond ((player? target)
           (if (member target (location-players location))
               (if (equal? target player)
                   (player-info player "you try in vain to hit yourself.")
                   (player-hits-player player target))
               (player-info player "you swipe at nothing.")))
          ((monster? target)
           (if (member target (location-monsters location))
               (player-hits-monster player target)
               (player-info player "that target is not here")))
          (#t (player-info player "attacking thin air makes you feel silly")))))

(define (player-grab player id)
  (let ((location (player-location player)))
    (if (location-has-item? location id)
        (let ((item (location-get-item location id)))
          (player-grabs-item player location item))
        (player-format player "I think you need glasses, there is no ~a here." id))))

(define (player-describe player id)
  (let ((location (player-location player))
        (thing (dungeon-get-identified id)))
    (cond ((and (monster? thing) (member thing (location-monsters location)))
           (player-format player (monster-description thing)))
          ((and (item? thing) (member thing (location-items location)))
           (player-format player "You inspect the ~a, but don't see anything special." (item-type-name (item-type* thing))))
          (else
           (player-format player "I think you need glasses, there is no ~a here." id)))))

(define (player-score player)
  (let* ((profile (player-profile player))
         (knowledge (profile-knowledge profile))
         (overall (apply + (map cdr ideal-profile)))
         (current 0))
    (player-format player "Your scores are as follows:")
    (player-format player "")
    (player-format player "-----------------------------------------------------")
    (player-format player "|         Topic            |  Required  |  Current  |")
    (player-format player "-----------------------------------------------------")
    (for/list ((topic ideal-profile))
      (let* ((name (car topic))
             (required-kills (cdr topic))
             (current-kills (cdr (assoc name knowledge)))
             (name-string (~a (symbol->string name) #:min-width 22 #:align 'center #:left-pad-string " " #:right-pad-string " "))
             (required-string (~a (number->string required-kills) #:min-width 8 #:align 'center #:left-pad-string " " #:right-pad-string " "))
             (current-string (~a (number->string current-kills) #:min-width 7 #:align 'center #:left-pad-string " " #:right-pad-string " ")))
        (player-format player "|  ~a  |  ~a  |  ~a  |" name-string required-string current-string)
        (set! current (+ current (min required-kills current-kills)))))
    (player-format player "-----------------------------------------------------")
    (player-format player "")
    (player-format player "You have achieved ~a out of ~a which is ~a%" current overall (round (* (/ current overall) 100)))))

(define (player-grabs-item player location item)
  (let ((event (make-event 'player-grabs-item location player item)))
    (set-location-items! location (remove item (location-items location)))
    (player-message player (grab-item (item-id item) (item-type* item)))
    (set-player-inventory! player (cons item (player-inventory player)))
    (raise-event event)))

(define (player-drop player id)
  (let ((location (player-location player)))
    (if (player-has-item? player id)
        (let ((item (player-get-item player id)))
          (player-drops-item player location item))
        (player-format player "You fumble around in your backpack, but you don't find ~a." id))))

(define (player-drops-item player location item)
  (let ((event (make-event 'player-drops-item location player item)))
    (set-location-items! location (cons item (location-items location)))
    (player-message player (drop-item (item-id item) (item-type* item)))
    (set-player-inventory! player (remove item (player-inventory player)))
    (raise-event event)))

(define (player-help player)
  (player-format player "                   Dungeons of Racket")
  (player-format player "------------------------------------------------------------")
  (player-format player "")
  (player-format player "The Dungeons of Racket were created many years ago by the")
  (player-format player "Grand Hacker who was bored of creating endless web-sites and")
  (player-format player "corporate database systems. As you wander around the rooms")
  (player-format player "of the Dungeons you will encounter many adversaries in the form")
  (player-format player "of topics from Computer Science. You must demonstrate your")
  (player-format player "skills by vanquishing all the topics in order to reach the")
  (player-format player "status of Novice First Class.")
  (player-format player "")
  (player-format player "When you start the game you are placed in a room within")
  (player-format player "the Dungeon. Rooms contain items that you can pick up and")
  (player-format player "use to help you. Rooms may also contain challenges that you")
  (player-format player "can solve using your knowledge of Computer Science.")
  (player-format player "")
  (player-format player "Unfortunately, the Dungeons are home to the pesky Computer")
  (player-format player "Science Topics. Each topic can be vanquished by attacking it.")
  (player-format player "Some of the items that you can pick up will help you in")
  (player-format player "your quest to vanquish a topic - otherwise brute force can")
  (player-format player "also be used.")
  (player-format player "")
  (player-format player "Each time to kill a Computer Science topic you add it to your")
  (player-format player "list of accomplishments. Your challenge is to get a complete")
  (player-format player "list of accomplishments while staying alive in the Dungeons.")
  (player-format player "")
  (player-format player "Good Luck!")
  (player-commands player))

(define (player-commands player)
  (player-format player "                      Game Commands")
  (player-format player "------------------------------------------------------------")
  (player-format player "")
  (player-format player "attack <id>     attack the topic with the given identifier.")
  (player-format player "commands        list the game commands.")
  (player-format player "describe <id>   describe the thing with the supplied identifier.")
  (player-format player "drop <id>       drop the item with the supplied identifier.")
  (player-format player "grab <id>       pick up the item with the supplied identifier.")
  (player-format player "help            get help on the game and its commands.")
  (player-format player "read runes      read runes on a wall (if present).")
  (player-format player "?               get help on the game and its commands.")
  (player-format player "move <dir>      leave the room in the specified direction.")
  (player-format player "score           describe how many topics you have mastered.")
  (player-format player "")
  (player-format player "When you grab an item it is placed in your inventory to show.")
  (player-format player "that it is being carried. Each item has its own collection of")
  (player-format player "commands that you can use when you are carrying it. These are")
  (player-format player "explained by the text displayed next to the item in your")
  (player-format player "inventory.")
  (player-format player "")
  (player-format player "To play the game type into the input area on the bottom left")
  (player-format player "of the game window. Don't delay - time marches on!"))

(define (player-hits-monster player monster)
  (let* ((location (player-location player))
         (profile (player-profile player))
         (weapon (profile-wielding profile))
         (damage (if weapon (random 5) (random 3)))
         (health (monster-health monster))
         (event (make-event 'player-hits-monster location (player-id player) (monster-id monster) damage)))
    (when (and (> health 0) (>= damage health))
      ; assume the monster is vanquished.
      ; acquire more knowlege of the topic.
      (let* ((profile (player-profile player))
             (knowledge (profile-knowledge profile))
             (monster-type (monster-type* monster))
             (topic (assoc monster-type knowledge))
             (knowledge* (cons (cons (car topic) (add1 (cdr topic))) (remove topic knowledge))))
        (set-profile-knowledge! profile knowledge*)))
    (raise-event event)))

(define (player-hits-player player other-player)
  ;;; The player has hit another player. 
  ;;; Raise an event so that receivers can take appropriate action....
  (let* ((other-id (player-id other-player))
         (location (player-location player))
         (id (player-id player))
         (damage (random 3))
         (event (make-event 'player-hits-player location id other-id damage)))
    (raise-event event)))

(define (handle-player-event player event)
  (log "[PLAYER-EVENT] ~a ~a ~a~%" (player-id player)  (event-name event) (location-id (event-location event)))
  (match (event-name event)
    ('player-leaves-location
     (let ((leaving-player (car (event-args event)))
           (room (event-location event))
           (dir (cadr (event-args event))))
       (when (not (equal? (player-id player) (player-id leaving-player)))
         (handle-player-leaves-room-event player leaving-player room dir))))
    ('player-enters-location
     (let ((arriving-player (car (event-args event)))
           (room (event-location event))
           (dir (cadr (event-args event))))
       (when (not (equal? (player-id player) (player-id arriving-player)))
         (handle-player-enters-room-event player arriving-player room dir))))
    ('monster-leaves-room
     (let ((monster (car (event-args event)))
           (location (event-location event))
           (exit (cadr (event-args event))))
       (when (same-location? location (player-location player))
         (handle-monster-leaves-room player monster location exit))))
    ('monster-enters-room
     (let ((monster (car (event-args event)))
           (location (event-location event))
           (exit (cadr (event-args event))))
       (when (same-location? (player-location player) location)
         (handle-monster-enters-room player monster location exit))
       (when (location-next-to? (player-location player) location)
         (handle-monster-enters-next-room player monster))))
    ('player-hits-player
     (let ((source-id (car (event-args event)))
           (target-id (cadr (event-args event)))
           (damage (caddr (event-args event))))
       (handle-player-hits-player player source-id target-id damage)))
    ('player-hits-monster
     (let ((source-id (car (event-args event)))
           (target-id (cadr (event-args event)))
           (damage (caddr (event-args event))))
       (handle-player-hits-monster player source-id target-id damage)))
    ('monster-dies
     (let ((id (car (event-args event)))
           (location (player-location player)))
       (when (memf (λ (monster) (equal? (monster-id monster) id)) (location-monsters location))
         (player-message player (monster-dies id)))))
    ('monster-hits-player 
     (let ((location (event-location event))
           (monster-id (car (event-args event)))
           (player-id (cadr (event-args event)))
           (damage (caddr (event-args event))))
       (handle-monster-hits-player player location player-id monster-id damage)))
    (else (printf "unknown event: ~a~%" event))))

(define (handle-monster-hits-player player location player-hit-id monster-id damage)
  (if (eq? (player-id player) player-hit-id)
      (let ((profile (player-profile player)))
        (set-profile-health! profile (- (profile-health profile) damage))
        (player-format player "You are hit by ~a!" monster-id)
        (when (<= (profile-health profile) 0)
          (player-score player)
          (player-format player "You die!")
          (player-quits player)))
      (when (eq? (player-location player) location)
        (player-format player "You see ~a hit ~a" monster-id player-hit-id))))

(define (handle-player-hits-player player source-id target-id damage)
  (if (equal? source-id (player-id player))
      (player-format player "you hit ~a" target-id)
      (if (equal? target-id (player-id player))
          (let ((profile (player-profile player)))
            (player-format player "~a hits you" source-id)
            (set-profile-health! profile (- (profile-health profile) damage)))
          (player-format player "~a hits ~a" source-id target-id))))

(define (handle-player-hits-monster player source-id target-id damage)
  (let* ((profile (player-profile player))
         (weapon-id (profile-wielding profile)))
    (if (zero? damage)
        (player-format player "you miss ~a" target-id)
        (if (equal? (player-id player) source-id)
            (if weapon-id
                (let ((weapon (player-get-item player weapon-id)))
                  (player-format player "you hit ~a with your ~s" target-id (item-type* weapon))
                  (when (chance? 5)
                    ; The weapon breaks.
                    (player-format player "The ~a breaks!" (item-type* weapon))
                    (set-profile-wielding! profile #f)
                    (set-player-inventory! player (remove weapon (player-inventory player)))
                    (player-message player (lose-item weapon-id))))
                (player-format player "you hit ~a with your bare hands." target-id))
            (if weapon-id
                (player-format player "~a hits ~a" source-id target-id)
                (player-format player "~a hits ~a with their bare hands." source-id target-id))))))

(define (handle-monster-enters-room player monster location exit)
  (let* ((text (monster-description monster))
         (text (string-append text " appears.")))
    (player-info player text)
    (player-message player (monster-in-room (monster-id monster) (monster-type* monster)))))

(define (handle-monster-enters-next-room player monster)
  (let ((type (monster-type* monster)))
    (player-format player "~a ~a snuffles nearby" (indefinite-article (symbol->string type)) type)))

(define (indefinite-article str)
  (if (vowel? (car (string->list str)))
      "an"
      "a"))

(define (vowel? char)
  (member char '(#\a #\e #\i #\o #\u)))

(define (handle-monster-leaves-room player monster location exit)
  (let* ((text (monster-description monster))
         (text (string-append text (format " leaves to the ~a." (exit-direction exit)))))
    (player-info player text)
    (player-message player (monster-leaves-room (monster-id monster)))))

(define (handle-player-enters-room-event player arriving-player room dir)
  (let* ((arriving-client (player-client arriving-player))
         (arriving-user (client-user arriving-client))
         (arriving-id (user-id arriving-user)))
    (player-format player "~a arrives in the room from ~a" arriving-id (direction-opposite dir))
    (player-message player (monster-in-room arriving-id 'player))))

(define (handle-player-leaves-room-event player leaving-player room dir)
  (let* ((leaving-client (player-client leaving-player))
         (leaving-user (client-user leaving-client))
         (leaving-id (user-id leaving-user)))
    (player-format player "~a leaves the room going ~a" leaving-id dir)
    (player-message player (monster-leaves-room leaving-id))))

;;; Processing a player occurs in a thread for that client. It 
;;; handles commands sent from the client and returns results to
;;; the client.

(define (player-processing player)
  (define (start-loop)
    (with-handlers
        ([(λ (x) #t) (λ (x) (log "[ERROR] ~a~%" x) (raise x))])
      (command-loop)))
  (define (command-loop)
    (when (> (profile-health (player-profile player)) 0)
      (let* ((client (player-client player))
             (command (client-read client)))
        (cond ((equal? command "quit")
               (client-quits client))
              ((string? command)
               (player-command player command)
               (command-loop))
              ((chat? command)
               (player-chat player command)
               (command-loop))
              ((runes-corrected? command)
               (player-corrects-runes player)
               (command-loop))
              (else (player-format player "unrecognised command: ~a" command))))))
  (log "[START-PLAYER] ~a~%" (player-id player))
  start-loop)

(define (player-corrects-runes player)
  (let* ((profile (player-profile player))
         (energy (profile-energy profile))
         (max-energy (profile-max-energy profile))
         (gain (+ (random 3) 1))
         (new-energy (max max-energy (+ energy gain))))
    (when (> new-energy energy)
      (set-profile-energy! profile new-energy)
      (player-format player "You gain ~a energy points." gain))))

(define (player-turn player)
  (let ((profile (player-profile player)))
    (profile-recovers-one-turn profile)
    (profile-expends-one-turn profile)
    (player-message player (update-turn turns))
    (player-message player (update-profile profile))
    (player-check-energy player)))

(define (profile-recovers-one-turn profile)
  ;;; Each turn the recovery counter is incremented until it reaches
  ;;; the number of turns necessary to recover a health point...
  (if (= (profile-recovery-counter profile)
         (profile-health-recovery-rate profile))
      (begin
        (set-profile-recovery-counter! profile 0)
        (unless (= (profile-health profile) (profile-max-health profile))
          (set-profile-health! profile (+ (profile-health profile) 1))))
      (set-profile-recovery-counter! profile (+ (profile-recovery-counter profile) 1))))

(define (profile-expends-one-turn profile)
  ;;; The energy is used up at a specified rate. The entergy counter is
  ;;; incremented until it reaches the specified rate and then the energy
  ;;; is decremented. When this reaches 0 the player is dead...
  (if (= (profile-energy-counter profile)
         (profile-energy-burn profile))
      (begin
        (set-profile-energy-counter! profile 0)
        (set-profile-energy! profile (- (profile-energy profile) 1)))
      (set-profile-energy-counter! profile (+ (profile-energy-counter profile) 1))))

;;; Usulity operations...

(define (chance? n)
  ;;; There is 1 in n chances of this returning true...
  (= (random n) 0))

(define (choose l)
  ;;; Choose an element from l at random...
  (list-ref l (random (length l))))

(define (raise-event event)
  (for ((player (dungeon-players)))
    (handle-player-event player event))
  (for ((monster (dungeon-monsters)))
    (handle-monster-event monster event)))

;;; Initialization...

(define dungeon (map* (void) '()))

(dungeon-start! 'r1 "a green room")

;(dungeon-add-location! 'r2 "a red room")
;(dungeon-add-location! 'r3 "a blue room")
;(dungeon-add-location! 'r4 "a white room")
;(dungeon-add-location! 'r5 "a black room")

;(location-connect! 'r1 'up "a dark corridor" 'r2)
;(location-connect! 'r1 'left "a low opening" 'r3)
;(location-connect! 'r3 'down "a forbidding cave" 'r4)
;(location-connect! 'r3 'up "a winding staircase" 'r5)
;(location-connect! 'r4 'left "an oak doorway" 'r1)

;(dungeon-add-item! (item 'programming-text-book 'c1 '((energy . 10) (weight . 5))) 'r1)

;(dungeon-add-item! (item 'gold 'g1 '((amount . 10) (weight . 10))) 'r1)

;(dungeon-add-item! (item 'lambda 's1 '((damage . 10) (weight . 10))) 'r1)

;(dungeon-add-item! (item 'armour 'a1 '((damage . 10) (weight . 20))) 'r1)
;(dungeon-add-item! (item 'armour 'a2 '((damage . 10) (weight . 20))) 'r1)

;(dungeon-add-monster! 'r1 (new-monster 'empty-set 'm1))
;(dungeon-add-monster! 'r1 (new-monster 'state-machine 'm2))

;(dungeon-add-monster! 'r2 (new-monster 'bat  'm1))
;(dungeon-add-monster! 'r5 (new-monster 'wolf 'w2))
;(dungeon-add-monster! 'r3 (new-monster 'bear 'b1))
;(dungeon-add-monster! 'r4 (new-monster 'lion 'l1))

;;; ***********************************************************************************************
;;; **************************** Clients and Networking Connectivity  *****************************
;;; ***********************************************************************************************

;;; The port to use for connecting to the server...
(define port 1234)

;;; Create a global listener for all clients...
(define listener '())

;;; A list of all the current clients...
(define clients '())

;;; A semaphore that is used to provide exclusive access to the
;;; server state when processing a client command....

(define semaphore (make-semaphore 1))

;;; Call listen in the listening thread to loop waiting
;; for clients...
(define (listen)
  (let-values
      (([from-client to-client] (tcp-accept listener)))
    (let* ((client (client (read-from from-client) from-client to-client))
           (profile (profile 
                     1     ; level
                     20    ; max-health 
                     20    ; health
                     10    ; health-recovery-rate
                     0     ; recovery-counter
                     5     ; strength
                     #f    ; invisible
                     10    ; energy
                     5     ; energy-burn
                     0     ; energy-counter
                     10    ; max-energy
                     10    ; armour-class
                     0     ; gold
                     '()   ; wearing nothing
                     #f    ; wielding nothing
                     (map (λ (topic) (cons (car topic) 0)) ideal-profile)))
           (player (player client profile '())))
      (client-message! client (ok))
      (add-client! player)
      (thread (player-processing player))
      (listen))))

(define (add-client! player)
  (semaphore-wait semaphore)
  (log "[ADD-CLIENT] ~a~%" (player-id player))
  (set! clients (cons (player-client player) clients))
  (start-player! player)
  (semaphore-post semaphore))

(define (start-player! player)
  (dungeon-add-player! player)
  (player-describe-room! player))

;;; Important to wrap serialization and deserialization around
;;; the reading and writing of data. In addition, make sure that
;;; output streams are flushed...

(define (read-from in)
  (deserialize (read in)))

(define (client-read client)
  (read-from (client-in client)))

(define (client-message! client message)
  (fprintf (client-out client) "~S" (serialize message))
  (log "[CLIENT-MESSAGE] ~a ~a~%" (client-user client) message)
  (flush-output (client-out client)))

(define (player-message player message)
  (client-message! (player-client player) message))

(define (player-format player format-string . args)
  (player-info player (apply format format-string args)))

(define (player-info player message)
  (player-message player (info message)))

;;; Clients should close themselves separately, but this can
;;; be used to shut them all down...
(define (close-all-clients)
  ;;; really should shut down the threads.
  (for ((client clients))
    (close-output-port (client-out client))
    (close-input-port (client-in client))))

;;; Tidy up...
(define (close-down)
  (semaphore-wait semaphore)
  (dungeon-delete-all-players!)
  (dungeon-kill-all-monsters!)
  (dungeon-stop-turns)
  (close-all-clients)
  (tcp-close listener)
  (set! listener '())
  (semaphore-post semaphore)
  (log "[CLOSE]~%")
  (stop-logging))

;; Start the server...
(define (startup)
  (start-logging "server.txt")
  (log "[START]~%")
  (dungeon-start-monsters)
  (dungeon-start-turns)
  (set! listener (tcp-listen port)))

;;; ***********************************************************************************************
;;; ***************************************  Behaviour  *******************************************
;;; ***********************************************************************************************

(struct behaviour (parser handler))

(define behaviours '())

(define (define-player-behaviour parser handler)
  (set! behaviours (cons (behaviour parser handler) behaviours)))

(define-player-behaviour
  (seq (literal "move") (var 'dir))
  (λ (player env)
    (let ((dir (lookup 'dir env)))
      (dungeon-move player (string->symbol dir)))))

(define-player-behaviour
  (seq (literal "attack") (var 'id))
  (λ (player env)
    (let ((id (lookup 'id env)))
      (player-attacks player (string->symbol id)))))

(define-player-behaviour
  (seq (literal "grab") (var 'id))
  (λ (player env)
    (let ((id (lookup 'id env)))
      (player-grab player (string->symbol id)))))

(define-player-behaviour
  (seq (literal "drop") (var 'id))
  (λ (player env)
    (let ((id (lookup 'id env)))
      (player-drop player (string->symbol id)))))

(define-player-behaviour
  (seq (literal "chat") (var 'id))
  (λ (player env)
    (let ((id (lookup 'id env)))
      (player-message player (get-chat-message (string->symbol id))))))

(define-player-behaviour
  (literal "help")
  (λ (player env)
    (player-help player)))

(define-player-behaviour
  (literal "?")
  (λ (player env)
    (player-help player)))

(define-player-behaviour
  (literal "commands")
  (λ (player env)
    (player-help player)))

(define-player-behaviour 
  (seq (literal "describe") (var 'id))
  (λ (player env)
    (let ((id (lookup 'id env)))
      (player-describe player (string->symbol id)))))

(define-player-behaviour
  (literal "score")
  (λ (player env)
    (player-score player)))

(define (delete-client! client)
  (semaphore-wait semaphore)
  (log "[DELETE-CLIENT] ~a~%" (client-user client))
  (set! clients (remove client clients equal?))
  (semaphore-post semaphore)) 

(define (player-quits player)
  (client-quits (player-client player)))

(define (client-quits client)
  (client-message! client (quit))
  (delete-client! client))

;;; ***********************************************************************************************
;;; ***************************************  Tests  ***********************************************
;;; ***********************************************************************************************

(define (test-server . init)
  (when (not (null? init)) (set! port (car init)))
  (startup)
  (with-handlers 
      ([(λ (x) true) 
        (λ (x) (log "[ERROR] ~a~%" x) (close-down) (raise x))])
    (listen)))

