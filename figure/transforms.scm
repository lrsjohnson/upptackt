;;; Rotations and translations (to points)

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
