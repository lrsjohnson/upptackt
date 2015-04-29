;;; mechanism.scm --- Group of Bars / Joints

;;; Commentary:

;; Ideas:
;; - Grouping of bars and joints
;; - Integrate with establishing toplogy

;; Future:
;; - Also specify constraints with it
;; - Convert to Diagram

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Mechanism Structure ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <m:mechanism>
    (%m:make-mechanism bars joints constraints
                       bar-table joint-table joint-by-vertex-table)
    m:mechanism?
    (bars m:mechanism-bars)
    (joints m:mechanism-joints)
    (constraints m:mechanism-constraints)
    (bar-table m:mechanism-bar-table)
    (joint-table m:mechanism-joint-table)
    (joint-by-vertex-table m:mechanism-joint-by-vertex-table))

(define (m:make-mechanism bars joints constraints)
  (let ((bar-table (m:make-bars-by-name-table bars))
        (joint-table (m:make-joints-by-name-table joints))
        (joint-by-vertex-table (m:make-joints-by-vertex-name-table joints)))
    (%m:make-mechanism bars joints constraints
                       bar-table joint-table joint-by-vertex-table)))

(define (m:mechanism . args)
  (let ((elements (flatten args)))
    (let ((bars (m:dedupe-bars (filter m:bar? elements)))
          (joints (filter m:joint? elements))
          (constraints (filter m:constraint? elements)))
      (m:make-mechanism bars joints constraints))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Deduplication ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (m:dedupe member-predicate elements)
  (cond ((null? elements) '())
        (else
         (let ((b1 (car elements)))
           (if (member-predicate b1 (cdr elements))
               (m:dedupe member-predicate (cdr elements))
               (cons b1 (m:dedupe member-predicate (cdr elements))))))))

(define (m:dedupe-bars bars)
  (m:dedupe (member-procedure m:bars-name-equivalent?) bars))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Accessors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:mechanism-joint-by-vertex-name m vertex-name)
  (m:find-joint-by-vertex-name
   (m:mechanism-joint-by-vertex-table m)
   vertex-name))

(define (m:mechanism-joint-by-names m dir-1-name vertex-name dir-2-name)
  (m:find-joint-by-names
   (m:mechanism-joint-table m)
   dir-1-name vertex-name dir-2-name))

(define (m:lookup m id)
  (cond ((m:bar-id? id) (m:find-bar-by-id
                         (m:mechanism-bar-table m)
                         id))
        ((m:joint-id? id) (m:find-joint-by-id
                           (m:mechanism-joint-table m)
                           id))
        ((m:joint-vertex-id? id) (m:find-joint-by-vertex-name
                                  (m:mechanism-joint-by-vertex-table m)
                                  (m:joint-vertex-id-name id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Specified ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:mechanism-fully-specified? mechanism)
  (every m:bar-fully-specified? (m:mechanism-bars mechanism))
  (every m:joint-fully-specified? (m:mechanism-joints mechanism)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Specify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Should these be in Linkages?

(define (m:specify-bar bar)
  (let ((v (m:random-bar-length)))
    (pp `(specifying-bar ,(print (m:bar-name bar)) ,v))
    (m:instantiate (m:bar-length bar) v 'specify-bar)))

(define (m:specify-joint joint)
  (let ((v (m:random-theta-for-joint joint)))
    (pp `(specifying-joint ,(print (m:joint-name joint)) ,v))
    (m:instantiate (m:joint-theta joint) v 'specify-joint)))

(define (m:specify-something m)
  ;; TODO: First try to specify a constraint
  (let ((bars (filter (notp m:bar-length-specified?)
                        (m:mechanism-bars m)))
        (joints (filter (notp m:joint-specified?)
                        (m:mechanism-joints m))))
    (let ((anchored-bars (filter m:bar-anchored? bars))
          (directioned-bars (filter m:bar-directioned? bars))
          (anchored-joints (filter m:joint-anchored? joints)))
      (let ((constrained-bars (filter m:constrained? anchored-bars))
            (constrained-joints (filter m:constrained? anchored-joints)))
        (cond
         ((not (null? constrained-joints))
          (m:specify-joint (car constrained-joints))
          #t)
         ((not (null? constrained-bars))
          (m:specify-bar (car constrained-bars))
          #t)
         ((not (null? anchored-joints))
          (m:specify-joint (car anchored-joints))
          #t)
         ((not (null? directioned-bars))
          (m:specify-bar (car directioned-bars))
          #t)
         ((not (null? anchored-bars))
          (m:specify-bar (car anchored-bars))
          #t)
         (else
          (let ((constrained-bars (filter m:constrained?
                                          bars))
                (specified-unanchored (filter m:joint-specified?
                                                (filter (notp m:joint-anchored?)
                                                        (m:mechanism-joints m))))
                (constrained-joints (filter m:constrained?
                                            joints)))
            (pprint specified-unanchored)
            (cond ((not (null? specified-unanchored))
                   (m:initialize-joint (car specified-unanchored)))
                  ((not (null? constrained-joints))
                   (m:initialize-joint (car constrained-joints)))
                  ((not (null? constrained-bars))
                   (m:initialize-bar (car constrained-bars)))
                  (else (m:initialize-bar (car bars))
                        ;(m:specify-bar (car bars))
                        )))
          #t))))))

;;;;;;;;;;;;;;;;;;;;;;;; Applying constraints ;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:apply-mechanism-constraints m)
  (for-each (lambda (c)
              (m:apply-constraint m c))
            (m:mechanism-constraints m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Build ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:identify-vertices m)
  (for-each (lambda (joints)
              (let ((first-vertex (m:joint-vertex (car joints))))
                (for-each (lambda (joint)
                            (m:identify-points first-vertex
                                               (m:joint-vertex joint)))
                          (cdr joints))))
            (hash-table/datum-list (m:mechanism-joint-by-vertex-table m))))

(define (m:build-mechanism m)
  (m:identify-vertices m)
  (m:assemble-linkages (m:mechanism-bars m)
                       (m:mechanism-joints m))
  (m:apply-mechanism-constraints m))

(define (m:solve-mechanism m)
  (let lp ()
    (run)
    (if (not (m:mechanism-fully-specified? m))
        (if (m:specify-something m)
            (lp)
            (error "Couldn't find anything to specify."))
        'mechanism-built)))

#|
 (begin
   (initialize-scheduler)
   (m:build-mechanism
    (m:mechanism
     (m:establish-polygon-topology 'a 'b 'c))))
|#

;;;;;;;;;;;;;;;;;;;;;;;; Conversion to Figure ;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:mechanism->figure m)
  (if (not (m:mechanism-fully-specified? m))
      (error "Can only convert fully specified mechanisms to Figures."))
  (let ((points
         (map (lambda (joint)
                       (m:point->figure-point (m:joint-vertex joint)))
              (m:mechanism-joints m)))
        (segments (map m:bar->figure-segment (m:mechanism-bars m)))
        (angles (map m:joint->figure-angle (m:mechanism-joints m))))
    (apply figure (append points segments angles))))

(define (m:draw-mechanism m c)
  (draw-figure (m:mechanism->figure m) c))

#|
(let lp ()
  (initialize-scheduler)
  (let ((m (m:mechanism
            (m:establish-polygon-topology 'a 'b 'c 'd))))
    (pp (m:joint-anchored? (car (m:mechanism-joints m))))
    (m:build-mechanism m)
    (m:solve-mechanism m)
    (let ((f (m:mechanism->figure m)))
      (draw-figure f c)
      (pp (analyze-figure f)))))
|#
