;;; direction-interval.scm --- Direction Intervals

;;; Commentary:

;; Ideas:
;; - Structure for representing ranges of directions
;; - Also interface for propagating partial information about angles
;; - Full circle intervals

;; Future:
;; - Multi-segment direction intervals
;; - Include direction? as direction-interval?
;; - Migrate additional direction/interval code from linkages.scm
;; - Deal with adding intervals to directions
;; - Clean up direction-interval intersection
;;   (subtract to start-1, e.g.)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Direction Intervals ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; "arc" of the circle from start-dir CCW to end-dir
;;; "invalid" allows for "impossible" intervals
(define-record-type <standard-direction-interval>
  (%make-standard-direction-interval start-dir end-dir)
  standard-direction-interval?
  (start-dir direction-interval-start)
  (end-dir direction-interval-end))

(define (make-direction-interval start-dir end-dir)
  (if (direction-equal? start-dir end-dir)
      (error "Cannot make direction-interval with no range:
 use direction or full interval"))
  (%make-standard-direction-interval start-dir end-dir))

(define (print-direction-interval di)
  `(direction-interval ,(direction-theta (direction-interval-start di))
                       ,(direction-theta (direction-interval-end di))))

(define (direction-interval-center di)
  (add-to-direction
   (direction-interval-start di)
   (/ (direction-interval-size di) 2.0)))

(defhandler print print-direction-interval standard-direction-interval?)

;;;;;;;;;;;;;;;;;;;; Invalid Direction Intervals ;;;;;;;;;;;;;;;;;;;;;

(define-record-type <invalid-direction-interval>
  (%make-invalid-direction-interval)
  invalid-direction-interval?)

(define (make-invalid-direction-interval)
  (%make-invalid-direction-interval))

(define (print-invalid-direction-interval di)
  `(invalid-direction-interval))
(defhandler print print-invalid-direction-interval invalid-direction-interval?)

;;;;;;;;;;;;;;;;;;;;;; Full Direction Intervals ;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <full-circle-direction-interval>
  (%make-full-circle-direction-interval)
  full-circle-direction-interval?)

(define (make-full-circle-direction-interval)
  (%make-full-circle-direction-interval))

(define (print-full-circle-direction-interval di)
  `(full-circle-direction-interval))
(defhandler print print-full-circle-direction-interval
  full-circle-direction-interval?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; All Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (direction-interval? x)
  (or (standard-direction-interval? x)
      (invalid-direction-interval? x)
      (full-circle-direction-interval? x)))

;;;;;;;;;;;;;;;;;;;;;;;;; More Constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-semi-circle-direction-interval start-dir)
  (make-direction-interval start-dir
                           (add-to-direction start-dir pi)))

(define (make-direction-interval-from-start-dir-and-size start-dir radians)
  (cond  ((or (close-enuf? radians (* 2 pi))
              (>= radians (* 2 pi)))
          (make-full-circle-direction-interval))
         ((close-enuf? radians 0)
          (error "cannot have interval of size 0: use direction"))
         ((< radians 0)
          (make-invalid-direction-interval))
         (else
          (make-direction-interval
           start-dir
           (add-to-direction start-dir radians)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Equality ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define direction-interval-equal?
  (make-generic-operation 2 'direction-interval-equal?))

(define (standard-direction-interval-equal? di1 di2)
  (and (direction-equal?
        (direction-interval-start di1)
        (direction-interval-start di2))
       (direction-equal?
        (direction-interval-end di1)
        (direction-interval-end di2))))

(defhandler direction-interval-equal?
  false-proc direction-interval? direction-interval?)

(defhandler direction-interval-equal?
  true-proc full-circle-direction-interval? full-circle-direction-interval?)

(defhandler direction-interval-equal?
  true-proc invalid-direction-interval? invalid-direction-interval?)

(defhandler direction-interval-equal?
  standard-direction-interval-equal?
  standard-direction-interval?
  standard-direction-interval?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Inclusion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define within-direction-interval?
  (make-generic-operation 2 'within-direction-interval?))

(define (within-standard-direction-interval? dir dir-interval)
  (let ((dir-start (direction-interval-start dir-interval))
        (dir-end (direction-interval-end dir-interval)))
    (or (direction-equal? dir dir-start)
        (direction-equal? dir dir-end)
        (< (subtract-directions dir dir-start)
           (subtract-directions dir-end dir-start)))))

(defhandler within-direction-interval?
  within-standard-direction-interval?
  direction?
  standard-direction-interval?)

(defhandler within-direction-interval?
  true-proc direction? full-circle-direction-interval?)

(defhandler within-direction-interval?
  false-proc direction? invalid-direction-interval?)

(define within-direction-interval-non-inclusive?
  (make-generic-operation 2 'within-direction-interval-non-inclusive?))

(define (within-standard-direction-interval-non-inclusive? dir dir-interval)
  (let ((dir-start (direction-interval-start dir-interval))
        (dir-end (direction-interval-end dir-interval)))
    (and (not (direction-equal? dir dir-start))
         (not (direction-equal? dir dir-end))
         (< (subtract-directions dir dir-start)
            (subtract-directions dir-end dir-start)))))

(defhandler within-direction-interval-non-inclusive?
  within-standard-direction-interval-non-inclusive?
  direction?
  standard-direction-interval?)

(defhandler within-direction-interval-non-inclusive?
  true-proc direction? full-circle-direction-interval?)

(defhandler within-direction-interval-non-inclusive?
  false-proc direction? invalid-direction-interval?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reverse-direction-interval
  (make-generic-operation 1 'reverse-direction-interval))

(define (reverse-standard-direction-interval di)
  (make-direction-interval
   (reverse-direction (direction-interval-start di))
   (reverse-direction (direction-interval-end di))))

(defhandler reverse-direction-interval
  reverse-standard-direction-interval
  standard-direction-interval?)

(defhandler reverse-direction-interval identity
  full-circle-direction-interval?)

(define direction-interval-size
  (make-generic-operation 1 'direction-interval-size))

(define (standard-direction-interval-size di)
  (subtract-directions (direction-interval-end di)
                       (direction-interval-start di)))

(defhandler direction-interval-size
  standard-direction-interval-size
  standard-direction-interval?)

(defhandler direction-interval-size
  (lambda (di) (* 2 pi))
  full-circle-direction-interval?)

;;; Rotate CCW by radians
(define shift-direction-interval
  (make-generic-operation 2 'shift-direction-interval))

(define (shift-standard-direction-interval di radians)
  (make-direction-interval
   (add-to-direction (direction-interval-start di) radians)
   (add-to-direction (direction-interval-end di) radians)))

(defhandler shift-direction-interval
  shift-standard-direction-interval
  standard-direction-interval? number?)

(defhandler shift-direction-interval
  (lambda (fcdi r) fcdi) full-circle-direction-interval? number?)

;;;;;;;;;;;;;;;;;; Direction interval intersection ;;;;;;;;;;;;;;;;;;;

(define intersect-direction-intervals
  (make-generic-operation 2 'intersect-direction-intervals))

(define (test-intersect-standard-dir-intervals di-1 di-2)
  (let ((result (internal-intersect-standard-dir-intervals di-1 di-2)))
    (let ((r-start (direction-interval-start result))
          (r-center (direction-interval-center result))
          (r-end (direction-interval-start result)))
      (if (not (and (within-direction-interval? r-start di-1)
                    (within-direction-interval? r-end di-1)
                    (within-direction-interval? r-center di-1)
                    (within-direction-interval? r-start di-2)
                    (within-direction-interval? r-center di-2)
                    (within-direction-interval? r-end di-2)))
          (error "Dir Intersection fail!"
                 (print (list di-1 di-2 result))))
      result)))

(define (intersect-standard-dir-intervals di-1 di-2)
  (let ((start-1 (direction-interval-start di-1))
        (end-1 (direction-interval-end di-1))
        (start-2 (direction-interval-start di-2))
        (end-2 (direction-interval-end di-2)))
    (if (> (direction-theta start-1)
           (direction-theta start-2))
        (intersect-standard-dir-intervals di-2 di-1)
        (cond
         ((or (direction-equal? start-2 start-1)
              (within-direction-interval-non-inclusive? start-2 di-1))
          ;; case 1: di-2 starts within di-1
          (if (within-direction-interval? end-1 di-2)
              (cond ((direction-equal? end-1 end-2)
                     (make-direction-interval start-2 end-2))
                    ;; Exclude the case where it loops around end ends
                    ;; within the start of di-1 again
                    ((within-direction-interval-non-inclusive? end-2 di-1)
                     nothing)
                    (else
                     (make-direction-interval start-2 end-1)))
              (make-direction-interval start-2 end-2)))
         ;; case 2: di-2 starts after di-1 and ends within di-1
         ((within-direction-interval? end-2 di-1)
          (make-direction-interval start-1 end-2))
         ;; case 3: di-2 starts after di-1 and ends beyond di-1
         ((or (within-direction-interval? end-1 di-2)
              (direction-equal? end-1 end-2))
          (make-direction-interval start-1 end-1))
         ;; Case 4: di-2 starts after di-1 and ends before di-1 starts again
         (else
          (pp (print (list di-1 di-2)))
          (error "No intersection")
          (make-invalid-direction-interval))))))

#|
;; Test cases
(define d0 (make-direction 0.))
(define d1 (make-direction 1.))
(define d2 (make-direction 2.))
(define d3 (make-direction 3.))
(define d4 (make-direction 4.))
(define d5 (make-direction 5.))
(define d6 (make-direction 6.))         ; almost all the way around

(define (test s1 e1 s2 e2)
  (print (intersect-standard-dir-intervals
    (make-direction-interval s1 e1)
    (make-direction-interval s2 e2))))

(test d0 d1 d0 d1)

|#

(defhandler intersect-direction-intervals
  (lambda (di idi) idi)
  direction-interval? invalid-direction-interval?)

(defhandler intersect-direction-intervals
  (lambda (idi di) idi)
  invalid-direction-interval? direction-interval?)

(defhandler intersect-direction-intervals
  (lambda (fcdi di) di)
  full-circle-direction-interval? direction-interval?)

(defhandler intersect-direction-intervals
  (lambda (di fcdi) di)
  direction-interval? full-circle-direction-interval?)

(defhandler intersect-direction-intervals
  intersect-standard-dir-intervals
  standard-direction-interval? standard-direction-interval?)


(define (intersect-direction-with-interval dir dir-interval)
  (if (within-direction-interval? dir dir-interval)
      dir
      (make-invalid-direction-interval)))

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

 (print-direction-interval
  (intersect-direction-intervals
   (make-direction-interval b d)
   (make-direction-interval f c)))
 ;Value: (dir-interval .7853981633974483 1.5707963267948966)

 etc.
|#

;;;;;;;;;;;;;; Direction Intervals as propagator values ;;;;;;;;;;;;;;

(defhandler equivalent? direction-interval-equal?
  direction-interval? direction-interval?)
(defhandler equivalent? (lambda (a b) #f)
  direction-interval? direction?)
(defhandler equivalent? (lambda (a b) #f)
  direction? direction-interval?)

(defhandler merge intersect-direction-intervals
  direction-interval? direction-interval?)
(defhandler merge intersect-direction-with-interval
  direction? direction-interval?)
(defhandler merge
  (lambda (di d)
    (intersect-direction-with-interval d di))
  direction-interval? direction?)
(defhandler merge
  (lambda (d1 d2)
    (if (direction-equal? d1 d2)
        d1
        (make-invalid-direction-interval)))
  direction? direction?)

(defhandler contradictory? invalid-direction-interval?
  direction-interval?)

;;;;;; Propagator generic operations on directions / intervals ;;;;;;;

(propagatify make-direction)

(define direction-sin (make-generic-operator 1 'direction-sin))

(defhandler direction-sin
  (lambda (d) nothing)
  direction-interval?)

(defhandler direction-sin
  (lambda (d) (sin (direction-theta d)))
  direction?)

(define direction-cos (make-generic-operator 1 'direction-cos))

(defhandler direction-cos
  (lambda (d) nothing)
  direction-interval?)

(defhandler direction-cos
  (lambda (d) (cos (direction-theta d)))
  direction?)

(propagatify direction-sin)

(propagatify direction-cos)
