;;; Rotations (to points)
;;; TODO: Make new angles?

(define (rotate-point-about rot-origin radians point)
  (let ((v (sub-points point rot-origin)))
    (let ((rotated-v (rotate-vec v radians)))
      (add-to-point rot-origin rotated-v))))

(define (rotate-segment-about rot-origin radians seg)
  (define (rotate-point p) (rotate-point-about rot-origin radians p))
  (segment (rotate-point (segment-p1 seg))
           (rotate-point (segment-p2 seg))))

(define (rotate-ray-about rot-origin radians r)
  (define (rotate-point p) (rotate-point-about rot-origin radians p))
  (ray (rotate-point (ray-p1 r))
       (rotate-point (ray-p2 r))))

(define (rotate-line-about rot-origin radians l)
  (define (rotate-point p) (rotate-point-about rot-origin radians p))
  (line (rotate-point (line-p1 l))
        (rotate-point (line-p2 l))))

(define rotate-about (make-generic-operation 3 'rotate-about))
(defhandler rotate-about rotate-point-about point? number? point?)
(defhandler rotate-about rotate-ray-about point? number? ray?)
(defhandler rotate-about rotate-segment-about point? number? segment?)
(defhandler rotate-about rotate-line-about point? number? line?)

(define (rotate-randomly-about p elt)
  (let ((radians (rand-angle-measure)))
    (rotate-about p radians elt)))


;;; Translations

(define (translate-point-by vec point)
  (add-to-point point vec))

(define (translate-segment-by vec segment)
  (define (translate-point p) (translate-point-by vec p))
  (segment (translate-point (segment-p1 seg))
           (translate-point (segment-p2 seg))))

(define (translate-ray-by vec r)
  (define (translate-point p) (translate-point-by vec p))
  (ray (translate-point (ray-p1 r))
       (translate-point (ray-p2 r))))

(define (translate-line-by vec l)
  (define (translate-point p) (translate-point-by vec p))
  (line (translate-point (line-p1 l))
        (translate-point (line-p2 l))))

(define (translate-angle-by vec a)
  (define (translate-point p) (translate-point-by vec p))
  (make-angle (angle-arm-1 a)
              (translate-point (angle-vertex a))
              (angle-arm-2 a)))

(define translate-by (make-generic-operation 2 'rotate-about))
(defhandler translate-by translate-point-by vec? point?)
(defhandler translate-by translate-ray-by vec? ray?)
(defhandler translate-by translate-segment-by vec? segment?)
(defhandler translate-by translate-line-by vec? line?)
(defhandler translate-by translate-angle-by vec? angle?)

(define (translate-randomly-along-line l elt)
  (let* ((vec (unit-vec (line->vec l)))
         (scaled-vec (scale-vec vec (rand-range 0.5 1.5))))
    (translate-by vec elt)))

(define (translate-randomly elt)
  (let ((vec (rand-translation-vec-for elt)))
    (translate-by vec elt)))

(define (rand-translation-vec-for-point p1)
  (let ((p2 (random-point)))
    (sub-points p2 p1)))
(define (rand-translation-vec-for-segment seg)
  (rand-translation-vec-for-point (segment-p1 seg)))

(define (rand-translation-vec-for-ray r )
  (rand-translation-vec-for-point (ray-p1 r)))

(define (rand-translation-vec-for-line l)
  (rand-translation-vec-for-point (line-p1 l)))

(define rand-translation-vec-for (make-generic-operation 1 'rand-translation-vec-for))
(defhandler rand-translation-vec-for rand-translation-vec-for-point point?)
(defhandler rand-translation-vec-for rand-translation-vec-for-segment segment?)
(defhandler rand-translation-vec-for rand-translation-vec-for-ray ray?)
(defhandler rand-translation-vec-for rand-translation-vec-for-line line?)
