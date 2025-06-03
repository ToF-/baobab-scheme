(load-option 'format)

(define MINIMAL 50.0)
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

(define (draw-squares p a l)
  (let* ((v (+ a (/ PI 2.0)))
         (ul (point (+ (x p) (* (cos v) l)) (+ (y p) (* (sin v) l))))
         (b (+ a (/ PI 4.0)))
         (d (* (sqrt 2.0) l))
         (ur (point (+ (x p) (* (cos b) d)) (+ (y p) (* (sin b) d))))
         (m (+ a (choose (/ PI 6.0) (/ PI 3.0))))
         (ll (* l (choose 0.4 0.9)))
         (t (point (+ (x ul) (* (cos m) ll)) (+ (y ul) (* (sin m) ll))))
         (n (compute-angle p ul))
         (lr (distance t ur)))
    (begin
      (format #t "// a: ~A p:~A  ul:~A  ur:~A  m:~A  ll:~A  t:~A  n:~A  lr:~A ~%" (deg a) p ul ur (deg m) ll t (deg n) lr)
      (draw-square p a l)
      (draw-square ul m ll)
      (draw-square t n lr)
      (move-to t)
      (line-to ur)
      )))

(define (print-html-canvas)
  (begin
  (print-html-canvas-header 1000 HEIGHT)
  (format #t "  ctx.beginPath();~%")
  (draw-squares (point 500.0 000.0) 0.0 100.0)
  (format #t "  ctx.stroke();~%")
  (print-html-canvas-footer)))

(define (main)
  (begin
    (print-html-canvas)
    (exit)))

