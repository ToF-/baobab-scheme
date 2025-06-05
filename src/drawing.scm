(load-option 'format)

(define MINIMAL 15.0)
(define HEIGHT 600.0)

(define PI 3.1415926535897932384)

(define seed (make-random-state #t))

(define (point x y)
  (cons x y))

(define (x p)
  (car p))

(define (y p)
  (cdr p))

(define (norm p)
  (sqrt (+ (square (x p)) (square (y p)))))

(define (normed-vector v)
  (let ((n (norm v)))
    (point (/ (x v) n) (/ (y v) n))))

(define (dot-product v w)
  (+ (* (x v) (x w)) (* (y v) (y w))))

(define (cross-product v w)
  (- (* (x v) (y w)) (* (y v) (x w))))

(define (compute-angle a b)
  (let ((n (normed-vector a))
        (m (normed-vector b)))
    (* (acos (dot-product n m))
        (if (<= (cross-product n m) 0) (- 1.0) 1.0))))

(define (normal-angle a)
  (if (< a 0.0)
        (normal-angle (+ a (* 2 PI)))
        a))

(define (angle-oriented ac ab ax)
  (let* ((abc (compute-angle ab ac))
         (abx (compute-angle ab ax)))
    (if (< abx 0)
      (normal-angle (- abc))
      (normal-angle abc))))

(define (print-angle a)
  (begin
    (display (/ (* a 180.0) PI))
    (newline)))

(define (rounded x)
    (/ (round (* 10000.0 x)) 10000.0))

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

(define (move-to p)
  (format #t "  ctx.moveTo(~A, ~A);~%"  (rounded (x p)) (- HEIGHT (rounded (y p)))))

(define (line-to p)
  (format #t "  ctx.lineTo(~A, ~A);~%" (rounded (x p)) (- HEIGHT (rounded (y p)))))

(define (draw-square p a l)
  (let* ((q (point (+ (x p) (* (cos a) l)) (+ (y p) (* (sin a) l))))
         (b (+ a (/ PI 4.0)))
         (d (* (sqrt 2.0) l))
         (r (point (+ (x p) (* (cos b) d)) (+ (y p) (* (sin b) d))))
         (v (+ a (/ PI 2.0)))
         (s (point (+ (x p) (* (cos v) l)) (+ (y p) (* (sin v) l)))))
    (begin
      (move-to p)
      (line-to q)
      (line-to r)
      (line-to s)
      (line-to p))))

(define (distance p q)
  (let ((dx (- (x q) (x p)))
        (dy (- (y q) (y p))))
    (sqrt (+ (square dx) (square dy)))))

(define (choose a b)
  (+ a (random (- b a) seed)))

(define (deg a)
  (* a (/ 180.0 PI)))

(define (vector p o)
    (point (- (x p) (x o)) (- (y p) (y o))))

(define (translate-with-angle origin angle size)
  (point (+ (x origin) (* (cos angle) size))
         (+ (y origin) (* (sin angle) size))))

(define (baobab origin angle size)
  (if (> size MINIMAL)
    (let* ((left-slope (+ angle (choose (/ PI 6.0) (/ PI 3.0))))
           (left-size (* size (choose 0.6 0.9)))
           (lower-right (translate-with-angle origin angle size))
           (upper-left (translate-with-angle origin (+ angle (/ PI 2.0)) size))
           (upper-right (translate-with-angle origin (+ angle (/ PI 4.0)) (* (sqrt 2.0) size)))
           (crotch (translate-with-angle upper-left left-slope left-size))
           (upper-vector (vector upper-left upper-right))
           (slope-vector (vector crotch upper-right))
           (right-slope (+ angle (angle-oriented slope-vector upper-vector (point 0.0 0.0))))
           (right-size (distance crotch upper-right)))
      (begin
        (move-to origin)
        (line-to upper-left)
        (line-to upper-right)
        (line-to lower-right)
        (line-to origin)
        (baobab upper-left left-slope left-size)
        (baobab crotch right-slope right-size)
        ))
    ()))

(define (print-html-canvas)
  (begin
  (print-html-canvas-header 1000 HEIGHT)
  (format #t "  ctx.beginPath();~%")
  (baobab (point 500.0 000.0) (choose (- (/ PI 200)) (/ PI 20.0)) 130.0)
  (format #t "  ctx.stroke();~%")
  (print-html-canvas-footer)))

(define (main)
  (begin
    (print-html-canvas)
    (exit)))

