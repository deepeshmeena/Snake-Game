;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname new-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define score 0)
(define WIDTH 10)
(define segment (circle 10 "solid" "red"))

(define board (place-image (rectangle 400 50 "solid" "Yellow") 200 425 (empty-scene 400 450)))

(define low (square 20 "solid" "Cyan"))
(define high (square 20 "solid" "DarkTurquoise"))

(define (loop x board)
  (cond [(> x 19) board]
        [else (loop (+ x 1) (help 0 x board))]))
  

(define (help i x board)
    (cond [(> i 19) board]
          [(odd? (+ x i)) (place-image high (* 10 (+ 1 (* 2 x))) (* 10 (+ 1 (* 2 i))) (help (+ i 1) x board))]
          [(even? (+ x i)) (place-image low (* 10 (+ 1 (* 2 x))) (* 10 (+ 1 (* 2 i))) (help (+ i 1) x board))]))
(define scn (loop 0 board))
(define food (circle 10 "solid" "blue"))

(define-struct game (snake food))
; A snake is a structure:
(define-struct snake (head tail dir))

(define game2 (make-game (make-snake (make-posn 110 170)
                         (list (make-posn 110 160) (make-posn 110 150))
                         "down")
                         (make-posn 310 310)))


;renders the game on the scene
(define (render-game g)
  
  (render-snake (game-snake g) (render-food g)))

; ; renders the food image on the scene
(define (render-food g)
  (place-image (text (number->string score) 25 "Sea Green") 170 425
               (place-image (text "Score: " 25 "Sea Green") 110 425
                            (place-image food (posn-x (game-food g)) (posn-y (game-food g))
               scn))))
;; The function renders the world on the scene
(define (render-snake w image)
  (cond [(hits-wall? w) (place-image (text (number->string score) 25 "Sea Green") 170 425
                                     (place-image (text "Score: " 25 "Sea Green") 110 425
                            (place-image (text (number->string score) 25 "Sea Green") 260 50
                                     (place-image (text "Your Score: " 25 "Sea Green") 160 50
                                                  (place-image (text "snake hits wall" 20 "black")
                                  100 300 image)))))]
        [(hits-itself? w (snake-tail w)) (place-image (text (number->string score) 25 "Sea Green") 170 425
                                                      (place-image (text "Score: " 25 "Sea Green") 110 425
                                                                   (place-image (text (number->string score) 25 "Sea Green") 260 50
                                                                                (place-image (text "Your Score: " 25 "Sea Green") 160 50
                                                                                             (place-image (text "snake hits itself" 20 "black")
                                                                                                          100 300 image)))))] 
        [else (render-head (snake-head w) (render-tail (snake-tail w) image))]))


(check-expect game2 (make-game (make-snake (make-posn 110 170)
                         (list (make-posn 110 160) (make-posn 110 150))
                         "down")
                         (make-posn 310 310)))

; changes the game to a new game based on the given KeyEvent
(define (key-game g ke)
  (make-game (key-snake (game-snake g) ke)
             (game-food g)))

;; changes the snake into a new snake based on the given KeyEvent
(define (key-snake w ke)
  (cond [(and (string=? (snake-dir w) "left") (key=? ke "right")) w]
        [(and (string=? (snake-dir w) "right") (key=? ke "left")) w]
        [(and (string=? (snake-dir w) "up") (key=? ke "down")) w]
        [(and (string=? (snake-dir w) "down") (key=? ke "up")) w]
        [(key=? ke "up") (if (is-valid (posn-x (snake-head w)) (posn-y (snake-head w)))
                             (make-snake (snake-head w)
                                    (snake-tail w)
                                    "up")
                         (make-snake (snake-head (tick-snake w))
                                    (snake-tail (tick-snake w))
                                    "up"))]
        [(key=? ke "down") (if (is-valid (posn-x (snake-head w)) (posn-y (snake-head w)))
                               (make-snake (snake-head w)
                                      (snake-tail w)
                                      "down")
                               (make-snake (snake-head (tick-snake w))
                                      (snake-tail (tick-snake w))
                                      "down"))]
        [(key=? ke "left") (if (is-valid (posn-x (snake-head w)) (posn-y (snake-head w)))
                               (make-snake (snake-head w)
                                          (snake-tail w)
                                          "left")
                               (make-snake (snake-head (tick-snake w))
                                      (snake-tail (tick-snake w))
                                      "left"))]
        [(key=? ke "right") (if (is-valid (posn-x (snake-head w)) (posn-y (snake-head w)))
                                (make-snake (snake-head w)
                                           (snake-tail w)
                                           "right")
                                (make-snake (snake-head (tick-snake w))
                                       (snake-tail (tick-snake w))
                                       "right"))]
        [else w]))


;changes the Game to a new Game
(define (tick-game g)
  (cond [(get-food? g (snake-head (game-snake g))) (lengthen g)]
        [else (tick-another-snake g)]))

; changes the game to a new game
(define (tick-another-snake g)
  (make-game (tick-snake (game-snake g))
             (game-food g)))
; changes the snake to a new snake 
(define (tick-snake w)
  (make-snake (tick-head (snake-head w) (snake-dir w))
             (tick-tail (snake-tail w) (snake-head w))
             (snake-dir w))) 

; determines whether the snake gets the food in the game
(define (get-food? g head)
  (cond 
        [(and (= (posn-x head) (posn-x (game-food g)))
              (= (posn-y head) (posn-y (game-food g))))
         (begin (set! score (+ 5 score))
         true)]
        [else false]))

; changes the head of snake to a new head
(define (tick-head head dir)
  (cond [(string=? dir "up") (make-posn (posn-x head) (- (posn-y head) WIDTH))] 
                                        
        [(string=? dir "down") (make-posn (posn-x head) (+ (posn-y head) WIDTH))]  
                                         
        [(string=? dir "left") (make-posn (- (posn-x head) WIDTH) (posn-y head))] 
                                         
        [(string=? dir "right") (make-posn (+ (posn-x head) WIDTH) (posn-y head))]))

; changes the tail of snake to a new tail 
(define (tick-tail tail head)
  (cond [(empty? tail) (cons head empty)]   
        [else (cons head (reverse (rest (reverse tail))))])) 
; A Game is a structure:
; (make-game snake food)
;(define-struct game (snake food))




; renders the head on the scene
(define (render-head head image)
  (place-image segment (posn-x head) (posn-y head) image))
; renders the tail on the scene
(define (render-tail tail scn)
  (cond [(empty? tail) scn]
        [else (place-image segment (posn-x (first tail)) (posn-y (first tail))
               (render-tail (rest tail) scn))]))

(define (is-valid x y)
  (and (odd? (quotient x 10)) (odd? (quotient y 10))))
;------------------------------------------------------------------------------------
;create a food which do not overlap with snake
(define (straightening wo)
  (cons (snake-head wo) (snake-tail wo))) 
(define (new-food-pos w)
  (let* [(x (* 10 (+ (* 2 (random 20))1)))
         (y (* 10 (+ (* 2 (random 20))1)))]
    (if (not-overlap (make-posn x y) w)
        (make-posn x y)
        (new-food-pos w))))

(define (not-overlap p w)
  (cond [(null? w) true]
        [(equal? (car w) p) false]
        [else (not-overlap p (cdr w))]))
;------------------------------------------------------------------------------------
; changes the game to a new game
(define (lengthen g) 
  (make-game (make-snake (make-new-head (game-snake g))
                        (add-to-tail (snake-tail (game-snake g)) (snake-head (game-snake g)))
                        (snake-dir (game-snake g)))
             (new-food-pos (straightening (game-snake g)))))
;----------------------------------------------------------------------------
; makes a new head
(define (make-new-head w)
  (cond [(string=? (snake-dir w) "up") (make-posn (posn-x (snake-head w)) 
                                                 (- (posn-y (snake-head w)) WIDTH))]
        [(string=? (snake-dir w) "down") (make-posn (posn-x (snake-head w))
                                                 (+ (posn-y (snake-head w)) WIDTH))]
        [(string=? (snake-dir w) "left") (make-posn (- (posn-x (snake-head w)) WIDTH)
                                                  (posn-y (snake-head w)))]
        [(string=? (snake-dir w) "right") (make-posn (+ (posn-x (snake-head w)) WIDTH)
                                                   (posn-y (snake-head w)))])) 
  
; add one more posn to the tail of the game
(define (add-to-tail tail head)
  (cons head tail))


 ; determines whether the snake hits the wall
(define (hits-wall? w)
  (if (or (< (posn-x (snake-head w)) 10) 
          (< (posn-y (snake-head w)) 10) 
          (> (posn-x (snake-head w)) 390) 
          (> (posn-y (snake-head w)) 390))
         true false)) 
; determines whether the snake hits itself
(define (hits-itself? w tail)
  (cond [(empty? tail) false]
        [(and (= (posn-x (snake-head w)) (posn-x (first tail)))
              (= (posn-y (snake-head w)) (posn-y (first tail))))
         true]
        [else (hits-itself? w (rest tail))]))


; game-ends: Game -> Boolean
; determines whether the game ends
(define (game-over g)
  (if (or (hits-wall? (game-snake g)) (hits-itself? (game-snake g) (snake-tail (game-snake g))))
      (begin
        (render-game g)
        (set! scn
              (place-image (text (number->string score) 25 "Sea Green") 170 425
                     (place-image (text "Score: " 25 "Sea Green") 110 425
                                  (place-image (text (number->string score) 25 "Sea Green") 260 50
                                               (place-image (text "Your Score: " 25 "Sea Green") 160 50
                                                            (place-image (text "Game Over" 20 "black")
                                                                         100 300 scn))))))
                                           true)
      false))


(big-bang game2 
          [to-draw render-game]
          [on-tick tick-game 0.1]
          [on-key key-game]
          [stop-when game-over]
          )
