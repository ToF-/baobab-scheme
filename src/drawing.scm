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

(define (draw-triangle x y angle unit)
  (let* ((half (/ unit 2.0))
         (xM (+ x (* (cos angle) half)))
         (yM (- y (* (sin angle) half)))
         (alpha (- angle (+ (/ PI 12.0) (random PI seed))))
         (xN (+ xM (* (cos alpha) half)))
         (yN (+ yM (* (sin alpha) half))))
    (begin
      (move-to x y)
      (line-to (+ x (* (cos angle) unit)) (- y (* (sin angle) unit)))
      (line-to xN yN)
      (line-to x y))))

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

(define (draw-square-and-triangle x y angle unit)
  (let* ((vert (+ angle (/ PI 2.0)))
         (xT (+ x (* (cos vert) unit)))
         (yT (- y (* (sin vert) unit))))
    (begin
      (draw-square x y angle unit)
      (draw-triangle xT yT angle unit))))

(define (print-html-canvas)
  (begin
  (print-html-canvas-header 1000 1000)
  (format #t "  ctx.beginPath();~%")
  (draw-square-and-triangle 500.0 1000.0 (/ PI 48.0) 300.0)
  (format #t "  ctx.stroke();~%")
  (print-html-canvas-footer)))

(define (main)
  (begin
    (print-html-canvas)
    (exit)))

