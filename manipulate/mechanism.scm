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

(define (m:print-mechanism m)
  `((bars ,(map print (m:mechanism-bars m)))
    (joints ,(map print (m:mechanism-joints m)))
    (constraints ,(map print (m:mechanism-constraints m)))))

(defhandler print m:print-mechanism m:mechanism?)

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

(define (m:multi-lookup m ids)
  (map (lambda (id) (m:lookup m id)) ids))

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
  (and (every m:bar-fully-specified? (m:mechanism-bars mechanism))
       (every m:joint-fully-specified? (m:mechanism-joints mechanism))))

(define (m:mechanism-contradictory? mechanism)
  (or (any m:bar-contradictory? (m:mechanism-bars mechanism))
      (any m:joint-contradictory? (m:mechanism-joints mechanism))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Specify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Should these be in Linkages?

(define *any-dir-specified* #f)
(define *any-point-specified* #f)

(define (any-one l)
  (let ((i (random (length l))))
    (list-ref l i)))

(define (m:pick-bar bars)
  (car (sort-by-key bars (negatep m:bar-max-inner-angle-sum))))

(define m:pick-joint-1 any-one)

(define (m:pick-joint joints)
  (car
   (append
    (sort-by-key
     (filter m:joint-bar-sums joints)
     m:joint-bar-sums)
    (filter (notp m:joint-bar-sums) joints))))

(define (m:specify-angle-if-first-time cell)
  (if (not *any-dir-specified*)
      (let ((dir (random-direction)))
        (set! *any-dir-specified* #t)
        (pp `(initializing-angle ,(name cell) ,(print dir)))
        (m:instantiate cell dir 'first-time-angle))))

(define (m:specify-point-if-first-time point)
  (if (not *any-point-specified*)
      (begin
        (set! *any-point-specified* #t)
        (pp `(initializing-point ,(name point) (0 0)))
        (m:instantiate-point point 0 0 'first-time-point))))

(define (m:specify-bar bar)
  (let ((v (m:random-bar-length)))
    (pp `(specifying-bar ,(print (m:bar-name bar)) ,v))
    (m:instantiate (m:bar-length bar) v 'specify-bar)
    (m:specify-angle-if-first-time (m:bar-direction bar))
    (m:specify-point-if-first-time (m:bar-p1 bar))))

(define (m:specify-joint joint)
  (let ((v (m:random-theta-for-joint joint)))
    (pp `(specifying-joint ,(print (m:joint-name joint)) ,v))
    (m:instantiate (m:joint-theta joint) v 'specify-joint)
    (m:specify-angle-if-first-time (m:joint-dir-1 joint))))

(define (m:initialize-joint-vertex joint)
  (m:specify-point-if-first-time (m:joint-vertex joint)))

(define (m:initialize-joint-direction joint)
  (m:specify-angle-if-first-time (m:joint-dir-1 joint)))

(define (m:initialize-bar-p1 bar)
  (m:specify-point-if-first-time (m:bar-p1 bar)))

(define (m:specify-joint-if m predicate)
  (let ((joints (filter (andp predicate (notp m:joint-specified?))
                        (m:mechanism-joints m))))
    (and (not (null? joints))
         (m:specify-joint (m:pick-joint joints)))))

(define (m:initialize-joint-if m predicate)
  (let ((joints (filter (andp predicate (notp m:joint-specified?))
                        (m:mechanism-joints m))))
    (and (not (null? joints))
         (let ((j (m:pick-joint joints)))
           (m:initialize-joint-direction j)))))

(define (m:specify-bar-if m predicate)
  (let ((bars (filter (andp predicate (notp m:bar-length-specified?))
                      (m:mechanism-bars m))))
    (and (not (null? bars))
         (m:specify-bar (m:pick-bar bars)))))

(define (m:initialize-bar-if m predicate)
  (let ((bars (filter (andp predicate (notp m:bar-length-specified?))
                      (m:mechanism-bars m))))
    (and (not (null? bars))
         (m:initialize-bar-p1 (m:pick-bar bars)))))

(define (m:specify-something m)
  (or
   (m:specify-bar-if m m:constrained?)
   (m:specify-joint-if m m:constrained?)
   (m:specify-joint-if m m:joint-anchored-and-arm-lengths-specified?)
   (m:specify-joint-if m m:joint-anchored?)
   (m:specify-bar-if m m:bar-directioned?)
   (m:specify-bar-if m m:bar-anchored?)
   (m:initialize-joint-if m m:joint-dirs-specified?)
   (m:initialize-bar-if m m:bar-length-dir-specified?)
   (m:initialize-bar-if m m:bar-direction-specified?)
   (m:initialize-bar-if m m:bar-length-specified?)
   (m:initialize-joint-if m m:joint-anchored?)
   (m:initialize-joint-if m true-proc)
   (m:initialize-bar-if m true-proc)))

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

(define (m:initialize-solve)
  (set! *any-dir-specified* #f)
  (set! *any-point-specified* #f))

(define *m* #f)
(define (m:solve-mechanism m)
  (set! *m* m)
  (m:initialize-solve)
  (let lp ()
    (run)
    (cond ((m:mechanism-contradictory? m)
           (m:draw-mechanism m c)
           (error "Contradictory mechanism built"))
          ((not (m:mechanism-fully-specified? m))
           (if (m:specify-something m)
               (lp)
               (error "Couldn't find anything to specify.")))
          (else 'mechanism-built))))

#|
 (begin
   (initialize-scheduler)
   (m:build-mechanism
    (m:mechanism
     (m:establish-polygon-topology 'a 'b 'c))))
|#

;;;;;;;;;;;;;;;;;;;;;;;; Conversion to Figure ;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:mechanism->figure m)
  (let ((points
         (map (lambda (joint)
                (m:point->figure-point (m:joint-vertex joint)))
              (m:mechanism-joints m)))
        (segments (map m:bar->figure-segment (m:mechanism-bars m)))
        (angles (map m:joint->figure-angle (m:mechanism-joints m))))
    (apply figure (flatten (filter (lambda (x) (or x))
                           (append points segments angles))))))

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
