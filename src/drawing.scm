(load-option 'format)

(define MINIMAL 50.0)

(define PI 3.1415926535897932384)

(define seed (make-random-state #t))

(define (point x y)
  (cons x y))

(define  (point-x p)
  (car p))

(define (point-y p)
  (cdr p))

(define (norm p)
  (sqrt (+ (square (point-x p)) (square (point-y p)))))

(define (normed-vector v)
  (let ((n (norm v)))
    (point (/ (point-x v) n) (/ (point-y v) n))))

(define (dot-product v w)
  (+ (* (point-x v) (point-x w)) (* (point-y v) (point-y w))))

(define (cross-product v w)
  (- (* (point-x v) (point-y w)) (* (point-y v) (point-x w))))

(define (compute-angle s1 s2)
  (let* ((n1 (normed-vector s1))
         (n2 (normed-vector s2))
         (cos-angle (dot-product n1 n2))
         (d (cross-product n1 n2))
         (angle (acos cos-angle)))
    (if (<= d 0)
      (- angle)
      angle)))

(define (print-angle a)
  (begin
    (display (/ (* a 180.0) PI))
    (newline)))

(define (r x)
  (begin
    (format #t "// ~A ~%" x)
    (/ (round (* 10000.0 x)) 10000.0)))

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
  (format #t "  ctx.moveTo(~A, ~A);~%"  (r x) (r y)))

(define (line-to x y)
  (format #t "  ctx.lineTo(~A, ~A);~%" (r x) (r y)))

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

(define (angle-m a b c)
  (let ((result (acos (/ (sqrt (+ (square a) (square b) (- (square c)))) (* 2 a b)))))
    (begin
      (format #t "// angle-m ~A ~A ~A → ~A ~%" a b c result)
      result)))

(define (draw-squares x y angle unit)
  (let* ((vert (+ angle (/ PI 2.0)))
         (x-upper-left (+ x (* (cos vert) unit)))
         (y-upper-left (- y (* (sin vert) unit)))
         (x-upper-right (+ x-upper-left (* (cos angle) unit)))
         (y-upper-right (- y-upper-left (* (sin angle) unit)))
         (alpha (choose (/ PI 6.0) (/ PI 1.2)))
         (left-unit (* unit (choose 0.4 0.9)))
         (angle-prime (+ angle (/ alpha 2.0)))
         (x-top (+ x-upper-left (* (cos angle-prime) left-unit)))
         (y-top (+ y-upper-left (* (sin angle-prime) left-unit)))
         (right-unit (distance x-top y-top x-upper-right y-upper-right))
         (beta (angle-m left-unit right-unit unit))
         (angle-second (+ angle beta)))
    ; h² = a² + o² → o² = h² - a² → o = √(h²-a²)

    (cond
      ((> unit MINIMAL)
       (begin
         (format #t "// ~A  ~A  ~A  ~A ~%" x y angle unit)
         (draw-square x y angle unit)
         (draw-squares x-upper-left y-upper-left angle-prime left-unit)
         (draw-squares x-top y-top angle-second right-unit) 
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
    (let ((p (point 5 5))
          (q (point 5 0)))
      (print-angle (compute-angle p q)))
    (let ((p (point 2 4))
          (q (point 4 2)))
      (print-angle (compute-angle p q)))
    (let ((p (point 1 0))
          (q (point 10 0)))
      (print-angle (compute-angle p q)))
    (let ((p (point 0 10))
          (q (point (- 10) 0)))
      (print-angle (compute-angle p q)))
;    (print-html-canvas)
    (exit)))

