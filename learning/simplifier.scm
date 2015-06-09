;;; simplifier.scm --- simplifies definitions

;;; Commentary:

;; Ideas:
;; - interfaces to manipulator

;; Future:
;; - Support more complex topologies.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;; Main Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (observations->constraints observations)
  (filter identity (map observation->constraint observations)))

(define (observation->constraint obs)
  (let ((rel (observation-relationship obs))
        (args (observation-args obs)))
    (let ((constraint-proc (relationship->constraint rel))
          (linkage-ids (args->linkage-ids args)))
      (and constraint-proc
           (every identity linkage-ids)
           (apply constraint-proc
                  (args->linkage-ids args))))))

(define (relationship->constraint rel)
  (case (relationship-type rel)
    ((equal-length) m:c-length-equal)
    ((equal-angle) m:c-angle-equal)
    (else #f)))

(define (args->linkage-ids args)
  (map arg->linkage-id args))

(define arg->linkage-id (make-generic-operation 1 'arg->linkage-id
                                                false-proc))

(define (segment->bar-id segment)
  (m:bar (element-name (segment-endpoint-1 segment))
         (element-name (segment-endpoint-2 segment))))
(defhandler arg->linkage-id segment->bar-id segment?)

(define (angle->joint-id angle)
  (m:joint (element-name (angle-vertex angle))))
(defhandler arg->linkage-id angle->joint-id angle?)

(define (establish-polygon-topology-for-n-gon n-sides)
  (cond ((= n-sides 3)
         (m:establish-polygon-topology 'a 'b 'c))
        ((= n-sides 4)
         (m:establish-polygon-topology 'a 'b 'c 'd))))

(define (observations->figure topology observations)
  (initialize-scheduler)
  (pprint (observations->constraints observations))
  (let ((m (apply
            m:mechanism
            (list
             topology
             (observations->constraints observations)))))
    (m:build-mechanism m)
    (if (not (m:solve-mechanism m))
        (begin
          (pp "Could not solve mechanism")
          #f)
        (let ((f (m:mechanism->figure m)))
          (pp "Solved!")
          (show-figure f)
          f))))

(define (topology-for-object obj)
  (if (polygon? obj)
      (establish-polygon-topology-for-n-gon
       (polygon-n-points obj))
      (error "Object isn't a polygon")))

;;; TODO: Make more general
(define (polygon-from-figure figure)
  (let ((all-points (figure-points figure)))
    (let lp ((i 1)
             (pts '()))
      (let ((p (find-point all-points
                           (nth-letter-symbol i))))
        (if p
            (lp (+ i 1)
                (append pts (list p)))
            (apply polygon-from-points pts))))))

(define (find-point points name)
  (let ((pts (filter
              (lambda (p)
                (eq? (element-name p) name))
              points)))
    (and (not (null? pts))
         (car pts))))
