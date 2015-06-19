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

(define (establish-polygon-topology-for-polygon polygon)
  (let* ((points (polygon-points polygon))
         (vertex-names (map element-name points)))
    (apply m:establish-polygon-topology vertex-names)))

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
      (establish-polygon-topology-for-polygon
       obj)
      (error "Object isn't a polygon")))

(define (polygon-from-new-figure point-names figure)
  (let* ((all-points (figure-points figure))
         (polygon-points
          (map
           (lambda (point-name)
             (find (lambda (p) (eq? (element-name p)
                                    point-name))
                   all-points))
           point-names)))
    (apply polygon-from-points polygon-points)))

(define (object-from-new-figure old-object figure)
  (if (polygon? old-object)
      (polygon-from-new-figure
       (map element-name (polygon-points old-object))
       figure)
      (error "Object isn't a polygon")))

;;;;;;;;;;;;;;;;;;;;; Simple Definitions Result ;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <simple-definitions-result>
  (%make-simple-definitions-result valid invalid unknown)
  simple-definitions-result?
  (valid simple-def-result-valid
         set-simple-def-result-valid!)
  (invalid simple-def-result-invalid
           set-simple-def-result-invalid!)
  (unknown simple-def-result-unknown
           set-simple-def-result-unknown!))

(define (make-simple-definitions-result)
  (%make-simple-definitions-result '() '() '()))

(define (mark-unknown-simple-def! def-result obs-subset)
  (set-simple-def-result-unknown! def-result
   (cons obs-subset  (simple-def-result-unknown def-result))))

(define (mark-invalid-simple-def! def-result obs-subset)
  (set-simple-def-result-invalid! def-result
   (cons obs-subset (simple-def-result-invalid def-result))))

(define (mark-valid-simple-def! def-result obs-subset)
  (set-simple-def-result-valid! def-result
   (cons obs-subset (simple-def-result-valid def-result))))

(define (simplify-definitions-result! def-result)
  (set-simple-def-result-valid! def-result
   (remove-supplants eq-subset?
                     (simple-def-result-valid def-result)))
  (set-simple-def-result-invalid! def-result
   (remove-supplants (flip-args eq-subset?)
                     (simple-def-result-valid def-result)))
  (set-simple-def-result-unknown! def-result
   (set-difference (simple-def-result-unknown def-result)
                   (simple-def-result-invalid def-result)
                   eq-subset?))
  (set-simple-def-result-unknown! def-result
   (set-difference (simple-def-result-unknown def-result)
                   (simple-def-result-valid def-result)
                   (flip-args eq-subset?))))

(define (print-simple-def-result def-result)
  (list (list 'valid (map print (simple-def-result-valid def-result)))
        (list 'invalid (map print (simple-def-result-invalid def-result)))
        (list 'unknown (map print (simple-def-result-unknown def-result)))))

(defhandler print
  print-simple-def-result
  simple-definitions-result?)
