;;; Math Helpers

;;; Angles

(define pi (* 4 (atan 1)))

(define (fix-angle-0-2pi a)
  (float-mod a (* 2 pi)))

(define (rad->deg rad)
  (* (/ rad (* 2 pi)) 360))

;;; Modular

(define (float-mod num mod)
  (- num
     (* (floor (/ num mod))
        mod)))

;;;  Basic Operators

(define (avg a b)
  (/ (+ a b) 2))

;;; Linear Alegbra

(define (det a11 a12 a21 a22)
  (- (* a11 a22) (* a12 a21)))

;;; Extensions of Max/Min

(define (min-positive . args)
  (min (filter (lambda (x) (>= x 0)) args)))

(define (max-negative . args)
  (min (filter (lambda (x) (<= x 0)) args)))
