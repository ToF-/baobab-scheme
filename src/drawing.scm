(load-option 'format)

(define PI 3.1415926535897932384)

(define seed (make-random-state #t))

(define (print-html-canvas-header width height)
  (format #t "<!DOCTYPE html>~%")
  (format #t "<html>~%")
  (format #t "<body>~%")
  (format #t "<canvas id=\"c\" width=\"~A\" height=\"~A\"></canvas>~%" width height)
  (format #t "<script>~%")
  (format #t "  const ctx = document.getElementById('c').getContext('2d');~%"))

(define (print-html-canvas-footer)
  (begin 
  (format #t "</script>~%")
  (format #t "</body>~%")
  (format #t "</html>~%")))

(define (move-to x y)
  (format #t "  ctx.moveTo(~A, ~A);~%" x y))

(define (line-to x y)
  (format #t "  ctx.lineTo(~A, ~A);~%" x y))

(define (draw-square x y angle unit)
  (let ((alpha (+ angle (/ PI 4.0)))
        (vert  (+ angle (/ PI 2.0)))
        (diag (* (sqrt 2.0) unit)))
    (begin
    (move-to x y)
    (line-to (+ x (* (cos angle) unit)) (- y (* (sin angle) unit)))
    (line-to (+ x (* (cos alpha) diag)) (- y (* (sin alpha) diag)))
    (line-to (+ x (* (cos vert) unit)) (- y (* (sin vert) unit)))
    (line-to x y))))

(define (distance x0 y0 x1 y1)
  (let ((dx (- x1 x0))
        (dy (- y1 y0)))
    (sqrt (+ (square dx) (square dy)))))

(define (choose a b)
  (+ a (random (- b a) seed)))

(define (draw-squares x y angle unit)
  (let* ((vert (+ angle (/ PI 2.0)))
         (x-upper-left (+ x (* (cos vert) unit)))
         (y-upper-left (- y (* (sin vert) unit)))
         (x-upper-right (+ x-upper-left (* (cos angle) unit)))
         (y-upper-right (- y-upper-left (* (sin angle) unit)))
         (alpha (choose (/ PI 6.0) (/ PI 1.2)))
         (angle-prime (+ angle (/ alpha 2.0)))
         (angle-second (- angle-prime (/ PI 2.0)))
         (half-unit (/ unit 2.0))
         (x-middle (+ x-upper-left (* (cos angle) half-unit)))
         (y-middle (- y-upper-left (* (sin angle) half-unit)))
         (x-top (+ x-middle (* (cos (+ angle alpha)) half-unit)))
         (y-top (- y-middle (* (sin (+ angle alpha)) half-unit)))
         (unit-prime (distance x-upper-left y-upper-left x-top y-top))
         (unit-second (distance x-top y-top x-upper-right y-upper-right)))
    ; h² = a² + o² → o² = h² - a² → o = √(h²-a²)

    (cond
      ((> unit 4.0)
       (begin
         (draw-square x y angle unit)
         (move-to x-upper-left y-upper-left)
         (draw-squares x-upper-left y-upper-left angle-prime unit-prime)
         (draw-squares x-top y-top angle-second unit-second) 
         ))
      (else ()))))

(define (print-html-canvas)
  (begin
  (print-html-canvas-header 1000 600)
  (format #t "  ctx.beginPath();~%")
  (draw-squares 500.0 600.0 (choose 0.0 (/ PI 24.0)) 100.0)
  (format #t "  ctx.stroke();~%")
  (print-html-canvas-footer)))

(define (main)
  (begin
    (print-html-canvas)
    (exit)))

