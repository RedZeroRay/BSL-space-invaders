;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ==========================================================

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

;; ==========================================================

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

;; -------------------------

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;; -------------------------

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; -------------------------

;; ListOfInvaders is one of:
;; - empty
;; - (cons (make-invader ListOfInvaders))

(define LOI1 (list I1))
(define LOI2 (list I1 I2))
(define LOI3 (list I1 I2 I3))

#;
(define (fn-for-loinvaders loi)
  (cond [(empty? loi) (...)]
        [else
         (...
          (fn-for-invader (first loi))
          (fn-for-loinvaders (rest loi)))]))

;; -------------------------                         

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; -------------------------

;; ListOfMissiles is one of:
;; - empty
;; - (cons (make-missile ListOfMissiles))

(define LOM1 (list M1))
(define LOM2 (list M1 M2))
(define LOM3 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))
                              

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ==========================================================

;; Functions:

;; Game -> Game

(define (main s)
  (big-bang s
    (on-tick   next-game)
    (to-draw   render-game)
    (on-key    handle-key)
    (stop-when game-over)))

;; -------------------------

;; Game -> Game
;; produces a filtered and ticked game state:
;;  change position of invaders
;;  remove invaders if hit
;;  produce new invader
;;  change position of missile
;;  remove missile if hit
;;  remove missile if offscreen
;;  change position of tank

(check-expect (next-game (make-game LOI1 LOM1 T0))
              (make-game
               (list (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 12))
               (list (make-missile 150 (- 300 MISSILE-SPEED)))
               (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (next-game (make-game (list (make-invader 140 100 12)) LOM2 T2))
              (make-game
               empty
               (list (make-missile 150 (- 300 MISSILE-SPEED)))
               (make-tank (- 50 TANK-SPEED) -1)))

;(define (next-game s) (make-game empty empty T0)) ;stub

(define (next-game s)
  (make-game (add-invader (tick-invaders (filter-invaders (game-invaders s) (game-missiles s))))
             (tick-missiles (filter-missiles (game-missiles s) (game-invaders s)))
             (tick-tank (game-tank s))))

;; -------------------------

;; ListOfInvaders -> ListOfInvaders
;; produces the next state of the ListOfInvaders

(check-expect (tick-invaders LOI1) (list(make-invader (+ 150 (* INVADER-X-SPEED 12)) (+ 100 INVADER-Y-SPEED) 12)))
(check-expect (tick-invaders LOI2)
              (list
               (make-invader (+ 150 (* INVADER-X-SPEED 12)) (+ 100 INVADER-Y-SPEED) 12)
               (make-invader (+ 150 (* INVADER-X-SPEED -10)) (+ HEIGHT INVADER-Y-SPEED) -10)))

;(define (tick-invaders loinvaders) (list empty)) ; stub

(define (tick-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons
          (tick-invader (first loi))
          (tick-invaders (rest loi)))]))

;; -------------------------

;; Invader -> Invader
;; moves the invader by one tick
(check-expect (tick-invader I1) (make-invader (+ 150 (* INVADER-X-SPEED 12)) (+ 100 INVADER-Y-SPEED) 12))
(check-expect (tick-invader (make-invader 300 100 12)) (make-invader (+ 300 (* INVADER-X-SPEED -12)) (+ 100 INVADER-Y-SPEED) -12))
(check-expect (tick-invader (make-invader 0 100 -12)) (make-invader (+ 0 (* INVADER-X-SPEED 12)) (+ 100 INVADER-Y-SPEED) 12))
                                
; (define (tick-invader i) (make-invader 0 0 12)) ; stub

(define (tick-invader invader)
  (cond [(> (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) WIDTH)
         (make-invader
          (+ (invader-x invader) (* INVADER-X-SPEED (- (invader-dx invader))))
          (+ (invader-y invader) INVADER-Y-SPEED)
          ( -(invader-dx invader)))]
        [(< (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) 0)
         (make-invader
          (+ (invader-x invader) (* INVADER-X-SPEED (- (invader-dx invader))))
          (+ (invader-y invader) INVADER-Y-SPEED)
          (- (invader-dx invader)))]
        [else (make-invader
               (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader)))
               (+ (invader-y invader) INVADER-Y-SPEED)
               (invader-dx invader))]))

;; -------------------------

;; ListOfMissiles -> ListOfMissiles
;; produces the next state of the ListOfMissiles
(check-expect (tick-missiles LOM1) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (tick-missiles (list
                              (make-missile 100 200)
                              (make-missile 250 50)))
              (list
               (make-missile 100 (- 200 MISSILE-SPEED))
               (make-missile 250 (- 50 MISSILE-SPEED))))

;(define (tick-missiles lom) (list empty)) ; stub

(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (tick-missile (first lom))
               (tick-missiles (rest lom)))]))

;; -------------------------

;; Missile -> Missile
;; moves the missile by one tick
(check-expect (tick-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))

;(define (tick-missile m) (make-missile 0 0)) ;stub

(define (tick-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; -------------------------

;; Tank -> Tank
;; produces the next state of the tank
(check-expect (tick-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (tick-tank T1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (tick-tank T2) (make-tank (+ 50 (- TANK-SPEED)) -1))
(check-expect (tick-tank (make-tank 0 -1)) (make-tank (+ 0 TANK-SPEED) 1))
(check-expect (tick-tank (make-tank WIDTH 1)) (make-tank (+ WIDTH (- TANK-SPEED)) -1))
                          
;(define (tick-tank tank) (make-tank 0 0)) ;stub

(define (tick-tank t)
  (cond
    [(< (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0)
     (make-tank (+ (tank-x t) (* (- (tank-dir t)) TANK-SPEED)) (- (tank-dir t)))]
    [(> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH)
     (make-tank (+ (tank-x t) (* (- (tank-dir t)) TANK-SPEED)) (- (tank-dir t)))]
    [else (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))

;; -------------------------

;; Game -> Game
;; filters out hit invaders and offscreen missiles
#;
(check-expect (filter-game (make-game LOI1 LOM1 T0))
              (make-game
               (list (make-invader 150 100 12))
               (list (make-missile 150 300))
               (make-tank (/ WIDTH 2) 1)))
#;
(check-expect (filter-game (make-game LOI1 LOM2 T0))
              (make-game
               empty
               (list M1)
               (make-tank (/ WIDTH 2) 1)))
 
;(define (filter-game s) (make-game empty empty T0)) ;stub
#;
(define (filter-game s)
  (make-game (filter-invaders (game-invaders s) (game-missiles s))
             (filter-missiles (game-missiles s) (game-invaders s))
             (game-tank s)))

;; -------------------------

;; ListOfInvaderes ListOfMissiles -> ListOfInvaders
;; destroys all hit invaders
(check-expect (filter-invaders LOI1 LOM1) LOI1)
(check-expect (filter-invaders LOI1 LOM2) empty)
(check-expect (filter-invaders LOI2 LOM2) (list I2))
(check-expect (filter-invaders LOI2 (list M3)) (list I2))

;(define (filter-invaders loi lom) empty) ;stub

(define (filter-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if
          (invader-hit? (first loi) lom)
          (filter-invaders (rest loi) lom)
          (append (list(first loi)) (filter-invaders (rest loi) lom)))]))

;; -------------------------

;; Invader ListOfMissiles -> Boolean
;; checks if a Invader is hit by a missile
(check-expect (invader-hit? I1 LOM1) false)
(check-expect (invader-hit? I1 LOM2) true)
(check-expect (invader-hit? I1 (list M3)) true)

;(define (invader-hit? i lom) empty) ;stub

(define (invader-hit? invader lom)
  (cond
    [(empty? lom) false]
    [else
     (or (hit? invader (first lom))
         (invader-hit? invader (rest lom)))]))

;; -------------------------

;; Invader Missile -> Boolean
;; checks if invader is hit by a missile
(check-expect (hit? I1 M1) false)
(check-expect (hit? I1 M2) true)
(check-expect (hit? I1 M3) true)
(check-expect (hit? (make-invader 100 100 10) (make-missile 100 100)) true)
(check-expect (hit? (make-invader 95 100 10) (make-missile 105 100)) true)
(check-expect (hit? (make-invader 95 95 10) (make-missile 105 105)) true)
(check-expect (hit? (make-invader 100 95 10) (make-missile 100 95)) true)

; (define (hit? i m) false) ;stub

(define (hit? invader missile)
  (and
   (<= (abs (- (invader-x invader) (missile-x missile))) HIT-RANGE)
   (<= (abs (- (invader-y invader) (missile-y missile))) HIT-RANGE)))

;; -------------------------
;; ListOfInvaders -> ListOfInvaders
;; adds an invader at a random x position to the list of invaders
(check-random (add-invader empty) (cond [(> INVADE-RATE (random 600)) (list (make-invader (random WIDTH) 0 1))] [else empty]))
(check-random (add-invader LOI1) (cond [(> INVADE-RATE (random 600)) (append LOI1 (list (make-invader (random WIDTH) 0 1)))][else LOI1]))

; (define (add-invader loi) empty) ;stub

(define (add-invader loi)
  (cond [(> INVADE-RATE (random 3000))
         (append loi (list (make-invader (random WIDTH) 0 1)))]
        [else loi]))

;; -------------------------

;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; filters all missiles that hit an invader or are offscreen
(check-expect (filter-missiles (list (make-missile 50 -10)) LOI1) empty)
(check-expect (filter-missiles LOM1 LOI1) LOM1)
(check-expect (filter-missiles LOM2 LOI1) LOM1)

; (define (filter-missiles lom loi) empty) ;stub

(define (filter-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (or
              (missile-hit? (first lom) loi)
              (missile-off? (first lom)))
             (filter-missiles (rest lom) loi)
             (append (list (first lom)) (filter-missiles (rest lom) loi)))]))

;; -------------------------

;; Missile ListOfInvaders -> Boolean
;; produces true if the missile hit an invader
(check-expect (missile-hit? M1 LOI1) false)
(check-expect (missile-hit? M2 LOI2) true)

;(define (missile-hit? missile loi) false) ;stub

(define (missile-hit? missile loi)
  (cond
    [(empty? loi) false]
    [else
     (or (hit? (first loi) missile)
         (missile-hit? missile (rest loi)))]))

;; -------------------------

;; Missile -> Boolean
;; produces true if the missile is offscreen
(check-expect (missile-off? M1) false)
(check-expect (missile-off? (make-missile 50 -10)) true)

; (define (missile-off? missile) false) ;stub

(define (missile-off? missile)
  (< (missile-y missile) 0))

;; -------------------------

;; Game -> Image
;; produces the current state of game as an image
;; invader
;; missile
;; tank
(check-expect (render-game (make-game LOI1 LOM1 T1))
              (place-image INVADER 150 100
                           (place-image MISSILE 150 300
                                        (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))
(check-expect (render-game (make-game (list (make-invader 200 30 10) (make-invader 75 300 10) (make-invader 100 120 10)) LOM3 T2))
              (place-image INVADER 200 30  (place-image INVADER 75 300 (place-image INVADER 100 120
                                                                                    (place-image MISSILE 150 300 (place-image MISSILE 150 110 (place-image MISSILE 150 105
                                                                                                                                                           (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))))

;(define (render-game s) (empty-scene)) ;stub

(define (render-game s)
  (render-tank (game-tank s)
               (render-missiles (game-missiles s)
                                (render-invaders (game-invaders s) BACKGROUND))))

;; -------------------------

;; ListOfInvaders Image -> Image
;; renders the invaders on the given image
(check-expect (render-invaders LOI1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invaders LOI2 BACKGROUND) (place-image INVADER 150 100 (place-image INVADER 150 500 BACKGROUND)))

; (define (render-invaders loi img) BACKGROUND) ;stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-invader (first loi)
                        (render-invaders (rest loi) img))]))

;; -------------------------

;; Invader Image -> Image
;; places invader on a given image
(check-expect (place-invader I1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))

;(define (place-invader invader img) BACKGROUND) ;stub

(define (place-invader invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img))

;; -------------------------
 
;; ListOfMissiles Image -> Image
;; renders the missiles on the given image
(check-expect (render-missiles LOM1 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-missiles LOM2 BACKGROUND) (place-image MISSILE 150 300 (place-image MISSILE 150 110 BACKGROUND)))

;(define (render-missiles lom img) BACKGROUND) ;stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-missile (first lom)
                        (render-missiles (rest lom) img))]))

;; -------------------------

;; Missile Image -> Image
;; places invader on a given image
(check-expect (place-missile M1 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))

;(define (place-missile missile img) BACKGROUND) ;stub

(define (place-missile missile img)
  (place-image MISSILE (missile-x missile) (missile-y missile) img))

;; -------------------------

;; Tank Image -> Image
;; renders the tank on the background
(check-expect (render-tank T1 BACKGROUND) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t img) BACKGROUND) ;stub

(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) img))

;; -------------------------

;; Game KeyEvent -> Game
;; produces the next state of game after pressing a key
(check-expect (handle-key (make-game LOI1 LOM1 T1) "left") (make-game LOI1 LOM1 (make-tank 50 -1)))
(check-expect (handle-key (make-game LOI1 LOM1 T2) "right") (make-game LOI1 LOM1 (make-tank 50 1)))
(check-expect (handle-key (make-game LOI1 LOM1 T2) " ") (make-game LOI1 (append LOM1 (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2)))) T2))
              
;(define (handle-key s kevt) (make-game empty empty T0)) ;stub

(define (handle-key s kevt)
  (cond [(key=? kevt "left")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s)) -1))]
        [(key=? kevt "right")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s)) 1))]
        [(key=? kevt " ")
         (make-game (game-invaders s)
                    (add-missile (game-missiles s) (game-tank s))
                    (game-tank s))]
        [else s]))

;; -------------------------

;; ListOfMissiles Tank -> ListOfMissiles
;; adds a missile at location of the tank
(check-expect (add-missile empty (make-tank 50 1)) (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2))))
(check-expect (add-missile LOM1 (make-tank 100 1)) (append LOM1 (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2)))))

;(define (add-missile lom t) empty) ;stub

(define (add-missile lom t)
  (append lom (list (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2))))) 

;; -------------------------

;; Game -> Game
;; stops the game when an invader hits the bottom of the screen
(check-expect (game-over (make-game LOI1 LOM1 T1)) false)
(check-expect (game-over (make-game LOI2 LOM1 T1)) true)
(check-expect (game-over (make-game LOI3 LOM1 T1)) true)

;(define (game-over s) false) ;stub

(define (game-over s)
  (landed? (game-invaders s)))

;; -------------------------

;; ListOfInvaders -> Boolean
;; produces true one of the invader touched the ground
(check-expect (landed? LOI1) false)
(check-expect (landed? LOI2) true)
(check-expect (landed? LOI3) true)

;(define (landed? i) false) ;stub

(define (landed? loi)
  (cond [(empty? loi) false]
        [else
         (if
          (touchdown? (first loi))
          true
          (landed? (rest loi)))]))

;; -------------------------

;; Invader -> Boolean
;; produces true if the invader touched the ground

(check-expect (touchdown? I1) false)
(check-expect (touchdown? I2) true)
(check-expect (touchdown? I2) true)

;(define (touchdown? i) false) ;stub

(define (touchdown? i)
  (>= (invader-y i) HEIGHT))