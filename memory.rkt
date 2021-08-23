#lang racket

;; Implements a memory game

(require 2htdp/universe 
         2htdp/image
         (only-in utils/2htdp/image
                  image-center-x
                  image-center-y
                  on-board
                  load-images)
         (only-in utils/matrix
                  matrix
                  matrix-ref)
         (only-in utils/list
                  repeat-list-elements))


;;---------------------------------------------------------------
;; Global variables. 
;; These values are concerned with the game matrix.
;;---------------------------------------------------------------


(define BOARD-ROWS 4)
(define BOARD-COLS 5)

;; holds a single board matrix
(define BOARD #f)


;;---------------------------------------------------------------
;; Graphic variables. 
;; These values are concerned with the game image.
;;---------------------------------------------------------------

;; Sub-directory where pictures for the game are stored.
(define IMG-DIR "pictures")

(define SQUARE-SIZE 80)

;; image used with letter overlay
(define SQUARE (color-frame 'black (square SQUARE-SIZE 'solid 'tan)))

;; image used for empty square
(define EMPTY-SQUARE (color-frame 'black (square SQUARE-SIZE 'solid 'darkgreen)))

(define BOARD-LIST (repeat-list-elements 2 
                                         (take (load-images IMG-DIR
                                                            #:width SQUARE-SIZE
                                                            #:height SQUARE-SIZE)
                                               (quotient (* BOARD-ROWS BOARD-COLS)
                                                         2))))

;; A scene crops images placed within it.
(define WIDTH (* (+ BOARD-COLS 2) (image-width SQUARE)))
(define HEIGHT (* (+ BOARD-ROWS 2) (image-height SQUARE)))
(define MT (empty-scene WIDTH HEIGHT 'black))


;;---------------------------------------------------------------
;; Helper functions.
;; These functions are concerned with conversions between the game
;; image and the board matrix, and the creation and placement of 
;; the marble and board.
;;---------------------------------------------------------------


;; The world state. 
;;    rollback? - boolean indicate whether prev-sq and curr-sq need to be restored
;;                to 'hidden' status.
;;    prev-sq - the sq struct for the "first" selection of a pair of selections.
;;    curr-sq - the sq struct for the "second" selection of a pair of selections.
;;    sq-count - a count of all the squares still 'hidden'.
;;    board - the game board
(struct world 
  (rollback? prev-sq curr-sq sq-count board) 
  #:mutable 
  #:transparent)

;; The board square state.
;;    value - the alphabetic symbol of the square when it is no longer 'hidden'.
;;    revealed? - boolean indicating whether the square's value is 'hidden' or 
;;                revealed on the board image.
(struct sq 
  (value revealed?) 
  #:mutable 
  #:transparent)

;; Sets up a new board as a matrix of randomly shuffled sq structs.
(define (new-board)
  (set! BOARD (matrix (shuffle (map (λ (v) (sq v #f)) BOARD-LIST))
                      #:rows BOARD-ROWS
                      #:cols BOARD-COLS)))

;;---------------------------------------------------------------
;; on-tick functions.
;; These functions are concerned with clock-ticks and check whether
;; we need to rollback sq states.
;;---------------------------------------------------------------


;; check-for-rollback: ws -> ws
;; Cleans up after pair selections. 
(define (check-for-rollback ws)
  ; the first of our selection pair.
  (define PREV (world-prev-sq ws))
  
  ; the second of our selection pair.
  (define CURR (world-curr-sq ws))
  
  (cond
    ; We have a pair that needs to be rolled back. We return the world state
    ; and the states of the pair of sq to their pre-selection status.
    [(world-rollback? ws)
     ; world state changes
     (set-world-rollback?! ws #f)
     (set-world-prev-sq! ws #f)
     (set-world-curr-sq! ws #f)
     (set-world-sq-count! ws (+ (world-sq-count ws) 2))
     
     ; sq state changes
     (set-sq-revealed?! PREV #f)
     (set-sq-revealed?! CURR #f)
     (sleep 1) ; sleep so the player can view the selected pair.
     ws]
    
    ; We have a pair, but don't need to hide them, just set up the
    ; world for another pair selection.
    [(world-curr-sq ws)
     (set-world-prev-sq! ws #f)
     (set-world-curr-sq! ws #f)
     ws]
    
    ; we either have a single selection or haven't made one yet.
    [else ws]))


;;---------------------------------------------------------------
;; on-mouse functions.
;; These functions are concerned with capturing mouse-clicks and 
;; setting the world state to the appropriate value.
;;---------------------------------------------------------------


;; check-for-reveal: ws mouse -> ws?
;; The gist of this is to select matching pairsof squares using mouse clicks.
(define (check-for-reveal ws x y me)
  ; offset for board positions. Since mouse positions are calculated based on
  ; the cursor location relative to the image passed to to-draw we have to adjust
  ;; the postions to that of the board within the MT scene.
  (set! x (- x (image-width SQUARE)))
  (set! y (- y (image-height SQUARE)))
  
  ; the first sq of our selection pair. This was captured from a previous
  ; mouse click.
  (define PREV (world-prev-sq ws))
  
  ; Captures whether the click was 'on the board', and where.
  (define-values (ON-BOARD? ROW COL) 
    (on-board x 
              y 
              (image-width SQUARE)
              (image-height SQUARE)
              (world-board ws)))
  
  ; The current square selected by the mouse click.
  (define CURR (if ON-BOARD? (matrix-ref (world-board ws) ROW COL) #f))
  
  ; Indicates whether the current selection square has already been revealed.
  (define REVEALED? (if CURR (sq-revealed? CURR) #f))
  
  (cond
    ; 'Selection' means a button-down click. We haven't made the second selection
    ; previously, and our selection is on the board and not yet revealed. This also
    ; means we can't select the same square as both a first and second selection.
    [(and (string=? me "button-down") 
          (not (world-curr-sq ws)) 
          ON-BOARD? 
          (not REVEALED?))
     (reveal-square ws PREV CURR)]
    
    ; anything else just returns the world asis.
    [else ws]))

;; reveal-square: world prev curr -> world?
;; Sets the states of the selected pair.
(define (reveal-square ws prev curr)
  ; reveal the currently selected square
  (set-sq-revealed?! curr #t)
  
  ; reduct the hidden square count by one.
  (set-world-sq-count! ws (sub1 (world-sq-count ws)))
  
  (cond
    ; Both squares match, save the selection as "current" and tell the
    ; process that we don't need to roll back the selections.
    [(and prev (equal? (sq-value prev) (sq-value curr)))
     (set-world-curr-sq! ws curr)
     (set-world-rollback?! ws #f)]
    
    ; The pair of squares don't match, sve the selection as "current" and
    ; tell the process we need to roll back the selections.
    [prev 
     (set-world-curr-sq! ws curr)
     (set-world-rollback?! ws #t)]
    
    ; This is our first selection of our pair, so we reveal it and save
    ; the selection as "previous", meaning we expect a follow-up.
    [else
     (set-sq-revealed?! curr #t)
     (set-world-prev-sq! ws curr)])
  
  ; return the world-state 
  ws)


;;---------------------------------------------------------------
;; to-draw functions.
;; These functions are concerned with drawing the game world. We
;; break the drawing process up into world, board, row, and square 
;; prpocesses. 
;;---------------------------------------------------------------


;; draw-memory-world: ws -> image?
;; Draws the game world.
(define (draw-memory-world ws)
  (define BOARD-SCENE (draw-board (world-board ws)))
  (place-image BOARD-SCENE
               (image-center-x MT)
               (image-center-y MT)
               MT))

;; draw-board: board -> image?
;; Draws the board based on the board matrix.
(define (draw-board board)
  (let loop ([board board] [acc empty])
    (cond
      [(null? board) (apply above (reverse acc))]
      [else (loop (cdr board) (cons (draw-board-row (car board)) acc))])))

;; draw-board-row: row -> image?
;; Draws a row of the board.
(define (draw-board-row row)
  (let loop ([row row] [acc empty])
    (cond
      [(null? row) (apply beside (reverse acc))]
      [else (loop (cdr row) (cons (draw-board-square (car row)) 
                                  acc))])))

;; draw-board-square s -> image?
;; Draws a board square representing the board's column and row element.
;; We use differnt colors to help effect some "movement" in selection.
(define (draw-board-square s)
  (define VAL (sq-value s))
  (define REVEALED? (sq-revealed? s))
  
  (cond
    [REVEALED? 
     (overlay
      (cond 
        [(symbol? VAL)
         (text (symbol->string (sq-value s))
               (quotient SQUARE-SIZE 2)
               'black)]
        [(image? VAL) VAL])
      SQUARE)]
    [else EMPTY-SQUARE]))


;;---------------------------------------------------------------
;; stop-when functions.
;; These functions are concerned with determining when the game
;; is over. 
;;---------------------------------------------------------------


;; board-complete? ws -> boolean?
;; Indicates whether there are no more 'hidden' squares on the board.
(define (board-complete? ws)
  (zero? (world-sq-count ws)))


;; star-game -> ws
;; Launches Big-Bang.
(define (start-game)
  (new-board)
  
  (big-bang
   ; No need to roll back, no selections, all the squares are hidden.
   (world #f #f #f (* BOARD-ROWS BOARD-COLS) BOARD)
   (on-tick check-for-rollback)
   (on-mouse check-for-reveal)
   (to-draw draw-memory-world)
   (stop-when board-complete?)
   (name "Memory"))
  
  (void))

(start-game)
