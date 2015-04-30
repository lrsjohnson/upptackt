;;; animation.scm --- Animating and persisting values in figure constructions

;;; Commentary:

;; Ideas:
;; - Animate a range
;; - persist randomly chosen values across frames

;; Future:
;; - Backtracking, etc.
;; - Save continuations?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;; Configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *animation-steps* 15)

;; ~30 Frames per second:
(define *animation-sleep* 30)

;;;;;;;;;;;;;;;;;;;;;;;;; Internal Constants ;;;;;;;;;;;;;;;;;;;;;;;;;
(define *is-animating?* #f)
(define *animation-value* 0)
(define *next-animation-index* 0)
(define *animating-index* 0)

(define (run-animation f-with-animations)
  (fluid-let ((*is-animating?* #t)
              (*persistent-values-table* (make-key-weak-eq-hash-table)))
   (let lp ((animate-index 0))
     (fluid-let
         ((*animating-index* animate-index))
       (let run-frame ((frame 0))
         (fluid-let ((*next-animation-index* 0)
                     (*next-value-index* 0)
                     (*animation-value*
                      (/ frame (* 1.0 *animation-steps*))))
           (f-with-animations)
           (sleep-current-thread *animation-sleep*)
           (if (< frame *animation-steps*)
               (run-frame (+ frame 1))
               (if (< *animating-index* (- *next-animation-index* 1))
                   (lp (+ animate-index 1))))))))))

;;;;;;;;;;;;;;;;;;;;;;;; Animating Functions ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; f should be a function of one float argument in [0, 1]
(define (animate f)
  (let ((my-index *next-animation-index*))
    (set! *next-animation-index* (+ *next-animation-index* 1))
    (f (cond ((< *animating-index* my-index) 0)
             ((= *animating-index* my-index) *animation-value*)
             ((> *animating-index* my-index) 1)))))

(define (animate-range min max)
  (animate (lambda (v)
             (+ min
                (* v (- max min))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Persistence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *persistent-values-table* #f)
(define *next-value-index* 0)

(define (persist-value v)
  (if (not *is-animating?*)
      v
      (let* ((my-index *next-value-index*)
             (table-value (hash-table/get
                           *persistent-values-table*
                           my-index
                           #f)))
        (set! *next-value-index* (+ *next-value-index* 1))
        (or table-value
            (begin
              (hash-table/put! *persistent-values-table*
                               my-index
                               v)
              v)))))
