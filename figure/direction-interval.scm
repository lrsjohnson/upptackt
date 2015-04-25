;;; direction-interval.scm --- Direction Intervals

;;; Commentary:

;; Ideas:
;; - Structure for representing ranges of directions
;; - Also interface for propagating partial information about angles

;; Future:
;; - Multi-segment direction intervals

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Direction Intervals ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; "arc" of the circle from start-dir CCW to end-dir
;;; "invalid" allows for "impossible" intervals
(define-record-type <direction-interval>
  (%make-direction-interval start-dir end-dir valid)
  direction-interval?
  (valid direction-interval-valid?)
  (start-dir direction-interval-start)
  (end-dir direction-interval-end))

(define (make-direction-interval start-dir end-dir)
  (%make-direction-interval start-dir end-dir #t))

(define (make-invalid-direction-interval)
  (%make-direction-interval 0 0 #f))

(define (make-semi-circle-direction-interval start-dir)
  (make-direction-interval start-dir
                           (add-to-direction start-dir pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (direction-interval-invalid? di)
  (not (direction-interval-valid? di)))

(define (direction-interval-equal? di1 di2)
  (and (eq? (direction-interval-valid? di1)
            (direction-interval-valid? di2))
       (direction-equal?
        (direction-interval-start di1)
        (direction-interval-start di2))
       (direction-equal?
        (direction-interval-end di1)
        (direction-interval-end di2))))

(define (within-direction-interval? dir dir-interval)
  (if (direction-interval-invalid? dir-interval)
      #f
      (let ((dir-start (direction-interval-start dir-interval))
            (dir-end (direction-interval-end dir-interval)))
        (<= (subtract-directions dir dir-start)
            (subtract-directions dir-end dir-start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (intersect-dir-intervals di-1 di-2)
  (if (or (direction-interval-invalid? di-1)
          (direction-interval-invalid? di-2))
      (make-invalid-direction-interval)
      (let ((start-1 (direction-interval-start di-1))
            (end-1 (direction-interval-end di-1))
            (start-2 (direction-interval-start di-2))
            (end-2 (direction-interval-end di-2)))
        (if (> (direction-theta start-1)
               (direction-theta start-2))
            (intersect-dir-intervals di-2 di-1)
            (cond
             ((within-direction-interval? start-2 di-1)
              (if (within-direction-interval? end-1 di-2)
                  (if (within-direction-interval? end-2 di-1)
                      (error "Can't handle duplicate Intersections")
                      (make-direction-interval start-2 end-1))
                  (make-direction-interval start-2 end-2)))
             ((within-direction-interval? end-2 di-1)
              (make-direction-interval start-1 end-2))
             (else (make-invalid-direction-interval)))))))


#|
 (define a (make-direction 0))
 (define b (make-direction (/ pi 4)))
 (define c (make-direction (/ pi 2)))
 (define d (make-direction pi))
 (define e (make-direction (* 3 (/ pi 2))))
 (define f (make-direction (* 7 (/ pi 4))))

 (within-direction-interval? b
  (make-direction-interval f c))
 ;Value: #t

 (within-direction-interval? b
  (make-direction-interval c f))
 ;Value: #f

 (define (print-interval di)
   (list 'dir-interval
         (direction-theta (direction-interval-start di))
         (direction-theta (direction-interval-end di))))

 (print-interval
  (intersect-dir-intervals
   (make-direction-interval b d)
   (make-direction-interval f c)))
 ;Value: (dir-interval .7853981633974483 1.5707963267948966)

 etc.
|#

;;;;;;;;;;;;;; Direction Intervals as propagator values ;;;;;;;;;;;;;;

(defhandler equivalent? direction-interval-equal?
  direction-interval? direction-interval?)

(defhandler merge intersect-dir-intervals
  direction-interval? direction-interval?)

(defhandler contradictory? direction-interval-invalid?
  direction-interval?)
