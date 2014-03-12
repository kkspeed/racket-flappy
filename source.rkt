(require 2htdp/image 2htdp/universe)

;; Physics
(define G 0.37)
(define TICK-RATE 0.08)
(define SPEED-BOOST 1.25)
(define BARRIER-PROB 0.2)
(define BARRIER-DIST 8)
(define OPENING 6)

;; World
(define SIZE 30)
(define SEG-SIZE 15)
(define WIDTH-PX  (* SEG-SIZE SIZE))
(define HEIGHT-PX (* SEG-SIZE SIZE))
(define MT-SCENE (overlay
                  (rectangle WIDTH-PX HEIGHT-PX "solid" (color 0 255 255 127))
                  (empty-scene WIDTH-PX HEIGHT-PX)))

;; Visual Constants
(define BIRD-IMG (bitmap "graphics/bird.gif"))
(define BRICK-IMG (bitmap "graphics/brick.gif"))
(define ENDGAME-TEXT-SIZE 32)
(define GAME-TEXT-SIZE 20)

(struct world (bird scene score) #:transparent)

(struct bird (vspeed x y))

(define (start-bird)
  (big-bang (world (bird 0 (quotient WIDTH-PX 2) (quotient SIZE 2))
                   (for/list ([i (range (add1 SIZE))])
                     (cons (make-opening 0 SIZE) i))
                   0)
            (to-draw render-world)
            (on-key boost-bird)
            (on-tick next-world TICK-RATE)
            (stop-when dead? render-end)))

(define (next-world w)
  (let ((bd (world-bird w))
        (scene (world-scene w)))
    (world (update-bird bd) (update-scene scene) (update-score w))))

(define (update-bird bd)
  (bird (bird-vspeed (increase-speed bd (- G))) (bird-x bd) (+ (bird-y bd) (bird-vspeed bd))))

(define (update-scene scene)
  (append
   (map (lambda (s i)
          (cons (car s) (add1 i))) (cdr scene) (range SIZE))
   (list (create-barrier scene))))

(define (update-score w)
  (let* ((score (world-score w))
         (bird (world-bird w))
         (mid (car (list-ref (world-scene w) (quotient SIZE 2)))))
    (if (and (special-opening? mid)
             (<= (car mid) (bird-y bird) (cadr mid)))
        (add1 score)
        score)))

(define (special-opening? o)
  (not (and (= (car o) 0) (= (cadr o) SIZE))))

(define (create-barrier scene)
  (if (and (flip-prob BARRIER-PROB)
           (andmap (lambda (p) (not (special-opening? (car p))))
                   (drop scene (- SIZE BARRIER-DIST))))
      (cons (make-opening (add1 (random (- SIZE OPENING))) OPENING) (add1 SIZE))
      (cons (make-opening 0 SIZE) (add1 SIZE))))

(define (make-opening start size)
  (list start (+ start size)))

(define (increase-speed bd v)
  (bird (+ (bird-vspeed bd) (- v)) (bird-x bd) (bird-y bd)))

(define (boost-bird w ke)
  (if (string=? ke "up")
      (world (increase-speed (world-bird w) SPEED-BOOST) (world-scene w)
             (world-score w))
      w))

(define (render-world w)
  (underlay/xy
   (render-bird (world-bird w)
                (render-scene (world-scene w) MT-SCENE))
   50 50
   (text (format "~a"
                 (world-score w)) GAME-TEXT-SIZE "red")))

(define (dead? w)
  (let* ((bird (world-bird w))
        (scene (world-scene w))
        (mid (car (list-ref scene (quotient SIZE 2)))))
    (not (<= (car mid) (bird-y bird) (cadr mid)))))

(define (render-end w)
  (overlay (text (format "Game over: ~a"
                         (world-score w)) ENDGAME-TEXT-SIZE "blue")
           (render-world w)))

(define (render-bird b scene)
  (place-image BIRD-IMG
               (bird-x b)
               (* (bird-y b) SEG-SIZE)
               scene))

(define (render-scene s scene)
  (foldr (lambda (i l)
           (draw-barrier (cdr i) (car i) 0 l))
         scene s))

(define (draw-barrier i opening n scene)
  (cond ((>= n SIZE) scene)
        ((<= (car opening) n (cadr opening)) (draw-barrier i opening (add1 n) scene))
        (else (draw-barrier i opening (add1 n)
                            (place-image BRICK-IMG
                                         (* i SEG-SIZE)
                                         (* n SEG-SIZE)
                                         scene)))))

(define (flip-prob prob)
  (< (random 100) (* 100 prob)))
