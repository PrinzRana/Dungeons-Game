#lang racket

(require (except-in racket/gui eval))
(require racket/serialize)
(require slideshow/pict)
(require framework)
(require racket/pretty)
(require "cell.rkt")
(require "images.rkt")
(require (only-in "parse.rkt" seq literal))
(require (only-in "logging.rkt" (log log*) start-logging stop-logging))

(provide client-tool%)

(define image-height 75)

(define text-height (inexact->exact (truncate (pict-height (text "XXX")))))

(define scroll-height 20)

(define command-style
  (send (make-object style-delta%) set-delta-foreground "red"))

(define response-style
  (send (make-object style-delta%) set-delta-foreground "blue"))

(define fixed-style
  (make-object style-delta% 'change-family 'modern))

(define runes%
  
  ;;; Runes are information that has been left for a player
  ;;; to complete. The runes are managed by the client.
  
  (class frame%
    (super-new
     [label "Runes"]
     [width 500]
     [height 300])
    (init-field runes)
    (init-field outcome)
    (define contents
      (new vertical-panel%
        [parent this]))
    (define editor
      (let* ((text (new text%))
             (canvas
              (new editor-canvas%
                [editor text]
                [parent contents]
                [style '(auto-vscroll auto-hscroll)])))
        (send text change-style fixed-style)
        canvas))
    (define/override (show showing)
      (if showing
          (begin
            (super show showing)
            (display-runes))
          (super show showing)))
    (define (display-runes)
      (match (runes-type runes)
        ('EXP (display-exp))
        ('FUN (display-fun))
        (else (display-error))))
    (define (restored-runes exp)
      (match (runes-type runes)
        ('EXP (restore-exp exp))
        ('FUN (restore-fun exp))
        (else (display-error))))
    (define (restore-exp exp)
      (with-handlers
          ([(λ (x) #t) (λ (x) (new error-dialog% [error x]))])
        (let ((value (eval exp (make-base-namespace))))
          (outcome (equal? value (runes-data runes))))))
    (define (restore-fun exp)
      (with-handlers
          ([(λ (x) #t) (λ (x) (new error-dialog% [error x]))])
        (let* ((data (runes-data runes))
               (call (car data))
               (result (cdr data))
               (value (eval `(begin ,exp ,call) (make-base-namespace))))
          (outcome (equal? value result)))))
    (define (display-exp)
      (clear)
      (display ";;; Someone (or something) has been writing ancient runes on the wall.~%")
      (display ";;; It looks like a Racket expression that evaluates to produce a value.~%")
      (display ";;; The runes are so old that some of then have faded. No doubt the~%");
      (display ";;; Racket Dwarves who mined the dungeon would be grateful if someone~%");
      (display ";;; restored the runes to their former glory.~%;;; The runes are as follows:~%~%")
      (display 
       (parameterize ([pretty-print-columns 40][print-as-expression #f])
         (pretty-format (runes-partial-code runes))))
      (display "~%;;; The value of the expression is: ~v~%" (runes-data runes)))
    (define (display format-string . args)
      (let ((text (send editor get-editor)))
        (send 
         text insert 
         (apply format format-string args) 
         (string-length (send text get-text)))))
    (define (clear)
      (let ((text (send editor get-editor)))
        (send text select-all)
        (send text clear)))
    (define (get-exp)
      (let ((text (send editor get-editor)))
        (with-handlers
            ([(lambda (x) #t)
              (lambda (x) (new error-dialog% [error x]))])
          (read (open-input-string (send text get-text))))))
    (define (display-fun)
      (clear)
      (display ";;; Someone (or something) has been writing ancient runes on the wall.~%")
      (display ";;; It looks like a Racket expression that evaluates to produce a value.~%")
      (display ";;; The runes are so old that some of then have faded. No doubt the~%");
      (display ";;; Racket Dwarves who mined the dungeon would be grateful if someone~%");
      (display ";;; restored the runes to their former glory.~%;;; The runes are as follows:~%~%")
      (display 
       (parameterize ([pretty-print-columns 40][print-as-expression #f])
         (pretty-format (runes-partial-code runes))))
      (display "~%~%;;; If you call: ~v~%" (car (runes-data runes)))
      (display "~%;;; The result is: ~v~%" (cdr (runes-data runes))))
    (define (display-error)
      (send editor set-value "error"))
    (define (close)
      (send this show #f))
    (define quit
      (new button%
        [parent contents]
        [label "Quit"]
        [callback (λ (b e) (close))]))
    (define submit
      (new button%
        [parent contents]
        [label "Restore Runes"]
        [callback (λ (b e) 
                    (close)
                    (restored-runes (get-exp)))]))))

(define error-dialog%
  (class dialog%
    (super-new
     [style '(close-button)]
     [label "Error"]
     [width 700]
     [height 300]
     [parent #f])
    (init-field error)
    (define error-message
      (new text-field% 
        [parent this] 
        [label #f] 
        [style '(multiple hscroll)]))
    (send error-message set-value (format "~v" error))
    (send this show #t)))

(define string-dialog%
  (class dialog%
    (super-new
     [style '(close-button)])
    (define ok #f)
    (define editor (new text-field% 
                     [parent this] 
                     [label #f] 
                     [style '(multiple hscroll)]))
    (define panel (new horizontal-panel% [parent this]))
    (define send-button 
      (new button% 
        [parent panel] 
        [label "send"] 
        [callback (λ (b e) (set! ok #t) (send this show #f))]))
    (define cancel-button 
      (new button% 
        [parent panel] 
        [label "cancel"] 
        [callback (λ (b e) (set! ok #f) (send this show #f))]))
    (define/public (get-string) (send editor get-value))
    (define/public (ok?) ok)))

(define output-area%
  (class text-field%
    [init-field text-size]
    [init-field text-style]
    (super-new
     [label #f]
     [font (make-object font% text-size 'modern text-style 'normal)]
     [style '(multiple)])
    ))

(define room-description%
  (class output-area%
    (super-new
     [text-size 16]
     [text-style 'italic]
     [min-height 120])
    (define/public (set-description text)
      (send this set-value text))))

(define item%
  (class object%
    (super-new)
    (init-field id)
    (init-field type)
    (define (bitmap)
      (match type
        ('cheese cheese)
        ('gold gold)
        ('sword sword)
        ('armour armour)
        ('lambda lambda-image)
        ('programming-text-book programming-text-book)
        ('drracket drracket)
        (else cheese)))
    (define drawer 
      (make-pict-drawer 
       (vc-append 
        (bitmap) 
        (text (string-append (symbol->string id) ":" (symbol->string type))))))
    (define/public (get-id) id)
    (define/public (get-type) type)
    (define/public (draw dc x) (drawer dc x 0))
    (define/public (get-width)
      (max (pict-width (bitmap)) (pict-width (text (string-append (symbol->string id) ":" (symbol->string type))))))))

(serializable-struct room
  (id x y description exits))

(serializable-struct corridor
  (dir source target))

(define map%
  ;;; A map is a 2-D array of rooms that dynamically grows as the
  ;;; client explores the dungeon. At any time the map is centred
  ;;; at position (x,y). The map is displayed in a fixed size and at a
  ;;; given zoom factor. These properties dictate what the user can see.
  (class canvas%
    (define offset-x 0)
    (define offset-y 0)
    (define zoom 1.0)
    (define zoom-increment 0.2)
    (define cell-width 50)
    (define cell-height 50)
    (define current-room-id 'r1)
    (define target-room-id #f)
    (define mode 'stationary)
    (define move-x 0)
    (define move-y 0)
    (define move-dir 'north)
    (define rooms (list (room 'r1 0 0 "a room" '(north south east west))))
    (define corridors '())
    (define selected-room #f)
    (define filling-brush (new brush% [style 'hilite] [color "Light Grey"]))
    (define exit-size 5)
    (define suspended-thread #f)
    
    ;;; **********************************************************************
    ;;; ************************ Display Operations **************************
    ;;; **********************************************************************
    
    (define (paint-me self dc)
      (send dc set-smoothing 'smoothed)
      ;;; Called by the GUI to paint the canvas. Display the graph of
      ;;; rooms and corridors...
      (for ((room rooms))
        (when (displayable-room? room)
          (display-room room dc)))
      (for ((corridor corridors))
        (when (displayable-corridor? corridor)
          (display-corridor corridor dc)))
      ;;; There are different modes of displaying the player....
      (match mode
        ('stationary
         ;;; Displayed in a single room...
         (display-position dc))
        ('moving
         ;;; Displayed as a sequence of animation frames to move
         ;;; the player from one room to another...
         (display-move dc))
        (else (display-position dc)))
      ;;; Mouse hovering?
      (display-selection dc))
    
    (define/public (player-moves source-id target-id dir description exits)
      
      ;;; Called in the client-tool thread when the player moves.
      ;;; We want to show an animation, so the client-tool thread is
      ;;; temporarily suspended while we go through a series of display
      ;;; refreshes each of which moves the player a step closer to the
      ;;; target room. This guarantees that the animation frame transitions
      ;;; are smooth. Once the animation is concluded, the client-tool
      ;;; thread is re-started...
      
      (let* ((source (get-room source-id))
             (target (virtual-get-room source dir target-id description exits)))
        
        ;;; Set up the variables that control moving. Change the client-tool
        ;;; mode to moving. This will change the update to process a sequence
        ;;; of animation frames...
        
        (set! target-room-id target-id)
        (set! mode 'moving)
        (set! move-x 0)
        (set! move-y 0)
        (set! move-dir dir)
        (send this refresh)
        
        ;;; The client-tool thread is suspended and will be re-started
        ;;; by the animation thread...
        
        (set! suspended-thread (current-thread))
        (thread-suspend (current-thread))))
    
    
    (define (display-move dc)
      
      ;;; Display a sequence of animation frames that show the player move 
      ;;; from their current location to the target location. This is achieved
      ;;; by sending a series of updates to the GUI thread. The position of
      ;;; the player is controlled by a pair of global variables...
      
      (if (or (>= (abs move-x) (* zoom 2 cell-width)) (>= (abs move-y) (* zoom 2 cell-height)))
          (begin
            ;;; We are finished and the player is now at the target
            ;;; location...
            (when suspended-thread
              ;;; Re-start the client-tool thread...
              (thread-resume suspended-thread)
              (set! suspended-thread #f))
            ;;; No longer moving...
            (set! mode 'stationary)
            (set! current-room-id target-room-id))
          
          ;;; Otherwise, draw the player at the current position which is
          ;;; somewhere on a straight line between the current room and the
          ;;; target room (scaled using the zoom factor)...
          
          (let* ((room (get-room current-room-id)))
            (let-values (((pict x y) (current-position room)))
              (draw-pict pict dc (+ x move-x) (+ move-y y))
              (match move-dir
                ('north (set! move-y (- move-y (* zoom 1))))
                ('east (set! move-x (+ move-x (* zoom 1))))
                ('west (set! move-x (- move-x (* zoom 1))))
                ('south (set! move-y (+ move-y (* zoom 1)))))
              
              ;;; Ensure that the refresh happens on the GUI thread.
              ;;; This will call display-me which will result in a ]
              ;;; recursive call to display-move...
              
              (queue-callback (λ() (send this refresh-now)))))))
    
    (define (display-position dc)
      
      ;;; Get the location of the player and display them on the
      ;;; canvas. The pict is returned suitably scaled...
      
      (let* ((room (get-room current-room-id)))
        (let-values (((pict x y) (current-position room)))
          (draw-pict pict dc x y))))
    
    (define (display-selection dc)
      
      ;;; The mouse has selected a particular room. Highlight the room
      ;;; and display some information about it...
      
      (when selected-room
        (let-values (((x y) (room-location selected-room)))
          (let ((width (/ (* cell-width zoom) 2))
                (height (/ (* cell-width zoom) 2))
                (brush (send dc get-brush)))
            (send dc set-brush filling-brush)
            (send dc draw-ellipse x y (* zoom cell-width) (* zoom cell-height))
            (send dc set-brush brush)
            (send dc draw-text (room-description selected-room) (+ x width) (+ y width))))))
    
    ;;; **********************************************************************
    ;;; ************************ Utility Operations **************************
    ;;; ********************************************************************** 
    
    (define (virtual-get-room source dir id description exits)
      
      ;;; The map expands dynamically. Requests for a room use this operation
      ;;; to return the room with the required id, creating it if necessary...
      
      (if (memf (λ (room) (eq? (room-id room) id)) rooms)
          (get-room id)
          (let* ((x (room-x source))
                 (y (room-y source))
                 (target-y (match dir ('north (- y 1)) ('south (+ y 1)) (else y)))
                 (target-x (match dir ('east (+ x 1)) ('west (- x 1)) (else x)))
                 (target (room id target-x target-y description exits)))
            (set! corridors (cons (corridor dir (room-id source) id) corridors))
            (set! rooms (cons target rooms))
            target)))
    
    (define (get-room-at x y)
      
      ;;; Return the room at the supplied position or #f...
      
      (let ((filtered (memf (λ (room) (room-contains? room x y)) rooms)))
        (if filtered
            (car filtered)
            filtered)))
    
    (define (room-contains? room x y)
      
      ;;; Used to check whether the room contains the (x,y) point.
      ;;; For example this could be used to check whether the mouse
      ;;; is over a room...
      
      (let-values (((room-x room-y) (room-location room)))
        (let ((width (* cell-width zoom))
              (height (* cell-height zoom)))
          (and (>= x room-x)
               (>= y room-y)
               (<= x (+ room-x width))
               (<= y (+ room-y height))))))
    
    (define (current-position room)
      (let* ((pict (scale player-on-map zoom))
             (width (* zoom cell-width))
             (height (* zoom cell-height))
             (p-width (pict-width pict))
             (p-height (pict-height pict)))
        (let-values (((x y) (room-location room)))
          (values
           pict 
           (+ x (- (/ width 2) (/ p-width 2)))
           (+ y (- (/ height 2) (/ p-height 2)))))))
    
    (define (room-location room)
      (let* ((r-x (+ (* zoom cell-width) (* (room-x room) (* zoom cell-width) 2)))
             (r-y (+ (* zoom cell-height) (* (room-y room) (* zoom cell-height) 2))))
        (values (+ r-x offset-x) (+ r-y offset-y))))
    
    (define (displayable-room? room)
      (let-values (((x y) (room-location room)))
        (and (> x 0) (> y 0) (< x (send this get-width)) (< y (send this get-height)))))
    
    (define (displayable-corridor? corridor)
      (let ((source (get-room (corridor-source corridor)))
            (target (get-room (corridor-target corridor))))
        (or (displayable-room? source) (displayable-room? target))))
    (define (get-room id)
      (car (memf (λ (room) (eq? (room-id room) id)) rooms)))
    (define (display-room room dc)
      (let-values (((x y) (room-location room)))
        (send dc draw-ellipse x y (* zoom cell-width) (* zoom cell-height))
        (display-room-exits room x y dc)))
    (define (display-room-exits room x y dc)
      (let ((exits (room-exits room)))
        (for ((exit exits))
          (display-room-exit room exit x y dc))))
    
    (define (display-room-exit room exit x y dc)
      (let ((width (* zoom cell-width))
            (height (* zoom cell-height)))
        (match exit
          ('north (display-exit (+ x (/ width 2)) y dc))
          ('south (display-exit (+ x (/ width 2)) (+ y height) dc))
          ('east (display-exit (+ x width) (+ y (/ height 2)) dc))
          ('west (display-exit x (+ y (/ height 2)) dc))
          (else '()))))
    (define (display-exit x y dc)
      (let ((exit-x (- x (floor (/ exit-size 2))))
            (exit-y (- y (floor (/ exit-size 2)))))
        (send dc draw-rectangle exit-x exit-y exit-size exit-size)))
    (define (display-corridor corridor dc)
      (let ((width (* cell-width zoom))
            (height (* cell-height zoom))
            (source (get-room (corridor-source corridor))))
        (let-values (((x y) (room-location source)))
          (match (corridor-dir corridor)
            ('north
             (send dc draw-line (+ x (/ width 2)) (- y height) (+ x (/ width 2)) y))
            ('south
             (send dc draw-line (+ x (/ width 2)) (+ y height) (+ x (/ width 2)) (+ y (* height 2))))
            ('east
             (send dc draw-line (+ x width) (+ y (/ height 2)) (+ x (* width 2)) (+ y (/ height 2))))
            ('west
             (send dc draw-line (- x width) (+ y (/ height 2)) x (+ y (/ width 2))))))))
    (super-new
     [paint-callback paint-me]
     [min-width 200]
     [min-height 200])
    (define drag-x 0)
    (define drag-y 0)
    (define/override (on-char event)
      (cond ((eq? (send event get-key-code) 'wheel-up)
             (set! zoom (min 2.0 (+ zoom zoom-increment)))
             (send this refresh))
            ((eq? (send event get-key-code) 'wheel-down)
             (set! zoom (max 0.2 (- zoom zoom-increment)))
             (send this refresh))
            (else '())))
    (define/override (on-event event)
      (cond ((send event button-down?)
             (let ((x (send event get-x))
                   (y (send event get-y)))
               (set! drag-x x)
               (set! drag-y y)))
            ((send event dragging?)
             (let ((x (send event get-x))
                   (y (send event get-y)))
               (set! offset-x (+ offset-x (- x drag-x)))
               (set! offset-y (+ offset-y (- y drag-y)))
               (set! drag-x x)
               (set! drag-y y)
               (send this refresh)))
            ((send event button-up?)
             (set! drag-x 0)
             (set! drag-y 0))
            (else 
             (let* ((x (send event get-x))
                    (y (send event get-y))
                    (room (get-room-at x y)))
               (if room
                   (set! selected-room room)
                   (set! selected-room #f))
               (send this refresh)))))))

(define items%
  (class canvas%
    (define (paint-me self dc)
      (let ((x 0))
        (for ((item items))
          (send item draw dc x)
          (set! x (+ x 10 (send item get-width))))))
    (super-new 
     [paint-callback paint-me]
     [min-width 200]
     [min-height (+ image-height text-height)]
     [stretchable-height #f]
     [style '()])
    (define items '())
    (define/public (contains? id)
      (memf  (λ (i) (equal? (send i get-id) id)) items))
    (define/public (add id type)
      (if (contains? id)
          (log* "[ERROR] item ~a already exists in display~%" id)
          (set! items (cons (new item% [id id] [type type]) items)))
      (send this refresh))
    (define/public (delete id)
      (if (contains? id)
          (let ((item (car (memf  (λ (i) (equal? (send i get-id) id)) items))))
            (set! items (remove item items))
            (send this refresh))
          (log* "[ERROR] cannot delete item ~a~%" id)))
    (define/public (clear)
      (set! items '())
      (send this refresh))))

(define inventory-item%
  (class object%
    (super-new)
    (init-field id)
    (init-field type)
    (define (bitmap)
      (match type
        ('cheese cheese)
        ('gold gold)
        ('sword sword)
        ('armour armour)
        ('lambda lambda-image)
        ('programming-text-book programming-text-book)
        ('drracket drracket)
        (else cheese)))
    (define (description)
      (match type
        ('cheese
         '("Cheese gives you energy and is light to carry."
           "Use EAT <ID> to consume cheese."
           "Use DROP <ID> to get rid of it."
           "Unfortunately it is rather smelly."))
        ('lambda 
         '("Lambda is a magical tool for defining functions."
           "You can use lambda in Racket to abstract arguments"
           "from an expression as in (lambda (x) (+ x 1)) which"
           "is the function that adds 1 to any argument."
           "Lambda is great for attacking any topic in Computer Science."
           "Use DROP <ID> to get rid of it."
           "Use WIELD <ID> to use it as a weapon."
           "Use UNWIELD <ID> to stop using it as a weapon."))
        ('gold
         '("Gold can be traded for things."
           "Use DROP <ID> to get rid of it."
           "Gold is quite heavy."))
        ('sword
         '("Swords are for combat (nasty pointy things)."
           "Use DROP <ID> to get rid of it."
           "Use WIELD <ID> to use it as a weapon."
           "Use UNWIELD <ID> to stop using it as a weapon."
           "The better the sword, the heavier it is."))
        ('programming-text-book
         '("Books are great for learning. This is a programming text-book."
           "Reading a text-book will give you extra energy for tackling"
           "those pesky Computer Science Topics."
           "Use READ <ID> to consume the knowledge."
           "Use DROP <ID> to get rid of it."))
        ('drracket
         '("Dr Racket is an environment that helps you to learn"
           "programming in the Racket language. It includes"
           "documentation and a debugger to help check on the."
           "troublesome features of the language."
           "Use DROP <ID> to get rid of it."
           "Use WIELD <ID> to use it as a weapon."
           "Use UNWIELD <ID> to stop using it as a weapon."))
        ('armour
         '("Armour protects you from being hit."
           "Use DROP <ID> to get rid of it."
           "Use WEAR <ID> to put the armour on."
           "Use REMOVE <ID> to take it off."
           "The more protective the armour, the heavier it is."))
        (else '("An unknown type of item."))))
    (define (explanation)
      (apply vl-append (map (λ (s) (text s 'modern)) (description))))
    (define pict 
      (ht-append
       (vc-append
        (bitmap)
        (text (format "~a:~a" id type) 'modern))
       (explanation)))
    (define drawer
      (make-pict-drawer pict))
    (define height (pict-height pict))
    (define/public (get-id) id)
    (define/public (get-type) type)
    (define/public (draw dc y) (drawer dc 0 y))
    (define/public (get-height) height)))

(define inventory%
  (class canvas%
    (define (paint-me self dc)
      (let ((y 0))
        (for ((item items))
          (send item draw dc y)
          (set! y (+ y (send item get-height))))))
    (super-new
     [paint-callback paint-me]
     [min-width 200]
     [min-height (+ image-height text-height)]
     [style '(hscroll vscroll)])
    (define items '())
    (define/public (contains? id)
      (memf  (λ (i) (equal? (send i get-id) id)) items))
    (define/public (add id type)
      (if (contains? id)
          (log* "[ERROR] item ~a already exists in display~%" id)
          (let ((item (new inventory-item% [id id] [type type])))
            (send this init-auto-scrollbars #f (* 200 (+ (length items) 1)) 0.0 0.0)
            (set! items (cons item items)))))
    (define/public (delete id)
      (if (contains? id)
          (let ((item (car (memf  (λ (i) (equal? (send i get-id) id)) items))))
            (set! items (remove item items))
            (send this refresh))
          (log* "[ERROR] cannot delete item ~a~%" id)))
    (define/public (clear)
      (set! items '())
      (send this refresh))
    (send this init-auto-scrollbars #f 200 0.0 0.0)))

(define monster%
  (class object%
    (super-new)
    (init-field id)
    (init-field type)
    (define (bitmap)
      (match type
        ('wolf wolf)
        ('bear bear)
        ('bat bat)
        ('lion lion)
        ('player player)
        ('empty-set empty-set)
        ('state-machine state-machine)
        ('arduino arduino)
        ('flip-flop flip-flop)
        ('tree tree)
        ('graph graph)
        ('proper-list proper-list)
        ('venn-diagram venn-diagram)
        ('hash-table hash-table)
        (else wolf)))
    (define drawer 
      (make-pict-drawer 
       (vc-append 
        (bitmap) 
        (text (string-append (symbol->string id) ":" (symbol->string type))))))
    (define/public (get-id) id)
    (define/public (get-type) type)
    (define/public (draw dc x) (drawer dc x 0))
    (define/public (get-width)
      (max (pict-width (bitmap)) (pict-width (text (string-append (symbol->string id) ":" (symbol->string type))))))))

(define monsters%
  (class canvas%
    (define (paint-me self dc)
      (let ((x 0))
        (for ((monster monsters))
          (send monster draw dc x)
          (set! x (+ x 10 (send monster get-width))))))
    (super-new 
     [paint-callback paint-me]
     [min-width 200]
     [min-height (+ image-height text-height)]
     [stretchable-height #f]
     [style '()])
    (define monsters '())
    (define/public (contains? id)
      (memf  (λ (i) (equal? (send i get-id) id)) monsters))
    (define/public (add id type)
      (if (contains? id)
          (log* "[ERROR] monster ~a already exists in display~%" id)
          (set! monsters (cons (new monster% [id id] [type type]) monsters))))
    (define/public (delete id)
      (if (contains? id)
          (let ((monster (car (memf  (λ (i) (equal? (send i get-id) id)) monsters))))
            (set! monsters (remove monster monsters))
            (send this refresh))
          (log* "[ERROR] cannot delete monster ~a~%" id)))
    (define/public (clear)
      (set! monsters '())
      (send this refresh))))

(define command-line%
  (class text-field%
    (super-new
     [label #f]
     [style '(single)]
     [callback 
      (λ (text-field event) 
        (when (eq? 'text-field-enter (send event get-event-type))
          (parse-command)))])
    (init-field handler)
    (define (parse-command)
      (let ((text (send this get-value)))
        (handler text)))))

(define property%
  (class text-field%
    (init-field name)
    (define property-label-width 20)
    (super-new 
     [stretchable-height #f]
     [font (make-object font% 12 'modern 'normal 'bold)]
     [label (~a name #:align 'left #:limit-marker "..." #:right-pad-string " " #:width property-label-width)])
    (define/public (get-name) name)))

(define profile%
  (class vertical-panel%
    (super-new)
    (define properties '())
    (define/public (set-profile profile)
      (add-property "level" (profile-level profile))
      (add-property "health" (profile-health profile))
      (add-property "recovery-counter" (profile-recovery-counter profile))
      (add-property "invisible" (profile-invisible profile))
      (add-property "strength" (profile-strength profile))
      (add-property "energy" (profile-energy profile))
      (add-property "energy-counter" (profile-energy-counter profile))
      (add-property "gold" (profile-gold profile))
      (add-property "wearing" (profile-wearing profile))
      (add-property "wielding" (profile-wielding profile))
      )
    (define (has-property? name)
      (memf (λ (p) (equal? name (send p get-name))) properties))
    (define (get-property name)
      (car (memf (λ (p) (equal? name (send p get-name))) properties)))
    (define (add-property name value)
      (if (has-property? name)
          (send (get-property name) set-value (format "~a" value))
          (let ((p (new property% 
                     [parent this] 
                     [name name] 
                     [init-value (format "~a" value)])))
            (set! properties (cons p properties)))))))

(define client-tool% 
  (class frame%
    ;;; The name of the user and the client id is supplied
    ;;; when creating the tool...
    (init-field user-name)
    (init-field id)
    
    ;;; The following input and output streams are used
    ;;; for communication with the server...
    (init-field from-server)
    (init-field to-server)
    
    ;;; Set the label of the window...
    (super-new [label (format "~a[0]" user-name)])
    
    ;;; The tool is a tree of elements, the following panels
    ;;; create the tree structure...
    (define h-panel 
      (new horizontal-panel% (parent this)))
    
    (define v-panel 
      (let ((panel (new vertical-panel% (parent h-panel))))
        panel))
    
    (define room
      (let ((box (new group-box-panel% [parent v-panel] [stretchable-height #f] [label "Room Description"])))
        (new room-description% [parent box])))
    
    (define monsters
      (let ((box (new group-box-panel% [parent v-panel] [stretchable-height #f] [label "Monsters In Room"])))
        (new monsters% [parent box])))
    
    (define items
      (let ((box (new group-box-panel% [parent v-panel] [stretchable-height #f] [label "Items In Room"])))
        (new items% [parent box])))
    
    (define history 
      ;;; The user is shown the history of events in a text area...
      (let ((box (new group-box-panel% [parent v-panel] [label "Event History"])))
        (new output-area% [text-size 14] [text-style 'normal] [parent box])))
    
    (define user-details
      ;;; The right-hand side of the display is the current state of the user...
      (new vertical-panel% (parent h-panel)))
    
    (define user-profile
      ;;; The user-profile is shown as a collection of properties...
      (let ((box (new group-box-panel% [stretchable-height #f] [parent user-details] [label "Profile"])))
        (new profile% [parent box])))
    
    (define map
      ;;; The user-profile is shown as a collection of properties...
      (let ((box (new group-box-panel% [parent user-details] [label "Map"])))
        (new map% [parent box])))
    
    (define inventory
      ;;; The users inventory is shown here...
      (let ((box (new group-box-panel% [parent user-details] [label "Inventory"])))
        (new inventory% [parent box])))
    
    (define runes #f)
    
    (define read-runes (seq (literal "read") (literal "runes")))
    
    (define (display-runes)
      (send runes show #t))
    
    (define (command-processor text) 
      ;;; When the user types a command it is supplied here and
      ;;; is sent to the server for processing...
      (command "~A~%" text)
      ;;; Handle local commands before sending to the server...
      (read-runes text 0 '() 
                  (λ (env pos) 
                    (if runes 
                        (display-runes) 
                        (command "I see no runes here!"))) 
                  (λ ()
                    (send-to-server text))))
    
    ;;; The user types a command into this...
    (define commands 
      (let ((box (new group-box-panel% [parent v-panel] [stretchable-height #f] [label "Commands"])))
        (new command-line% [parent box] [handler command-processor])))
    
    (define/public (register)
      ;;; Called to register the client with the server...
      (send-to-server (user user-name id))
      (if (ok? (read-from-server))
          (response "registered~%")
          (error "cannot register tool")))
    
    (define (set-turns turns)
      (send this set-label (format "~a[~a]" user-name turns)))
    
    (define/public (start)
      ;;; The GUI thread handles the commands sent to 
      ;;; the command-line, in addition the tool will
      ;;; read on the stream from the server and show the
      ;;; information in the output part of the display...
      (define (read-loop)
        (send this refresh)
        (let ((message (read-from-server)))
          (log* "[MESSAGE] ~a~%" message)
          (cond ((info? message)
                 (queue-callback (λ () (response "~a~%" (info-text message))))
                 (read-loop))
                ((room-info? message)
                 (queue-callback (λ() (send room set-description (room-info-text message))))
                 (read-loop))
                ((clear-monsters? message)
                 (queue-callback (λ() (send monsters clear)))
                 (read-loop))
                ((clear-items? message)
                 (queue-callback (λ() (send items clear)))
                 (read-loop))
                ((monster-in-room? message)
                 (queue-callback (λ() (send monsters add (monster-in-room-id message) (monster-in-room-type message))))
                 (read-loop))
                ((item-in-room? message)
                 (queue-callback (λ() (send items add (item-in-room-id message) (item-in-room-type message))))
                 (read-loop))
                ((player-in-room? message)
                 (queue-callback (λ() (send monsters add (player-in-room-id message) 'player)))
                 (read-loop))
                ((monster-leaves-room? message)
                 (queue-callback (λ() (send monsters delete (monster-leaves-room-id message))))
                 (read-loop))
                ((monster-dies? message)
                 (queue-callback 
                  (λ() 
                    (send monsters delete (monster-dies-id message))
                    (response "~a dies~%" (monster-dies-id message))))
                 (read-loop))
                ((update-turn? message)
                 (queue-callback (λ() (set-turns (update-turn-turns message))))
                 (read-loop))
                ((update-profile? message)
                 (queue-callback (λ() (send user-profile set-profile (update-profile-profile message))))
                 (read-loop))
                ((player-moves? message)
                 ;;; Get rid of the runes window if it exists...
                 (when runes
                   (send runes show #f)
                   (set! runes #f))
                 ;;; Animation is required so don't perform the multiple updates
                 ;;; on the window thread at this point...
                 (send map player-moves 
                       (player-moves-source-id message)
                       (player-moves-target-id message)
                       (player-moves-dir message)
                       (player-moves-description message)
                       (player-moves-exits message))
                 (read-loop))
                ((grab-item? message)
                 (queue-callback 
                  (λ () 
                    (send items delete (grab-item-id message))
                    (send inventory add (grab-item-id message) (grab-item-type message))))
                 (read-loop))
                ((drop-item? message)
                 (queue-callback
                  (λ ()
                    (send items add (drop-item-id message) (drop-item-type message))
                    (send inventory delete (drop-item-id message))))
                 (read-loop))
                ((lose-item? message)
                 (queue-callback (λ () (send inventory delete (lose-item-id message))))
                 (read-loop))
                ((quit? message)
                 #t)
                ((get-chat-message? message)
                 (let* ((id (get-chat-message-id message))
                        (dialog (new string-dialog% 
                                  [parent #f] 
                                  [min-width 400]
                                  [label (format "What do you want to say to ~a?" id)])))
                   (send dialog show #t)
                   (when (send dialog ok?)
                     (send-to-server (chat id (send dialog get-string))))
                   (read-loop)))
                ((room-runes? message)
                 (let ((data (room-runes-data message)))
                   (if data
                       (set! runes (new runes% [outcome rune-outcome] [runes data]))
                       (set! runes #f)))
                 (read-loop))
                (else 
                 (queue-callback (λ() (response "unknown message ~a~%" message)))
                 (read-loop)))))
      (response "[started]~%")
      (thread
       (λ ()
         (with-handlers
             ([(λ (x) #t)
               (λ (x) (log* "[ERROR] ~a~%" x) (stop-logging) (raise x))])
           (start-logging (format "~a.txt" user-name))
           (read-loop)
           (stop-logging))))
      )
    
    (define (rune-outcome correct?)
      (if correct?
          (begin
            (response "You have corrected the runes!~%")
            (response "You feel more enlightened.~%")
            (send-to-server (runes-corrected id)))
          (response "You fail to correct the runes.~%")))
    
    (define/public (send-to-server data)
      (log* "[COMMAND] ~a~%" data)
      (fprintf to-server "~S" (serialize data))
      (flush-output to-server))
    
    (define/public (read-from-server)
      (deserialize (read from-server)))
    
    (define/public (close)
      (close-input-port from-server)
      (close-output-port to-server))
    
    (define/public (command format-string . args)
      (let ((editor (send history get-editor)))
        (send editor change-style command-style)
        (send (send history get-editor) insert (apply format format-string args))))
    
    (define/public (response format-string . args)
      (let ((editor (send history get-editor)))
        (send editor change-style response-style)
        (send (send history get-editor) insert (apply format format-string args))))
    
    ))
