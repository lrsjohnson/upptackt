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
    (%m:make-mechanism bars joints constraints)
    m:mechanism?
    (bars m:mechanism-bars)
    (joints m:mechanism-joints)
    (constraints m:mechanism-constraints))

(define (m:make-mechanism bars joints constraints)
  (%m:make-mechanism bars joints constraints))

(define (m:mechanism args)
  (let ((elements (flatten args)))
    (let ((bars (filter m:bar? elements))
          (joints (filter m:joint? elements))
          (constraints (filter m:constraint? elements)))
      (m:make-mechanism bars joints constraints))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Specified ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:mechanism-fully-specified? mechanism)
  (every m:bar-fully-specified? (m:mechanism-bars mechanism))
  (every m:joint-fully-specified? (m:mechanism-joints mechanism)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Specify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Should these be in Linkages?

(define (m:specify-bar bar)
  (let ((v (m:random-bar-length)))
    (pp `(specifying-bar ,(m:bar-name bar) ,v))
    (m:instantiate (m:bar-length bar) v 'specify-bar)))

(define (m:specify-joint joint)
  (let ((v (m:random-theta-for-joint joint)))
    (pp `(specifying-joint ,(m:joint-name joint) ,v))
    (m:instantiate (m:joint-theta joint) v 'specify-joint)))

(define (m:specify-something mechanism)
  ;; TODO: First try to specify a constraint
  (let ((bars (filter (notp m:bar-length-specified?)
                        (m:mechanism-bars mechanism)))
        (joints (filter (notp m:joint-specified?)
                        (m:mechanism-joints mechanism))))
    (let ((anchored-bars (filter m:bar-anchored? bars))
          (directioned-bars (filter m:bar-directioned? bars))
          (anchored-joints (filter m:joint-anchored? joints)))
      (cond
       ((not (null? anchored-joints))
        (m:specify-joint (car anchored-joints))
        #t)
       ((not (null? anchored-bars))
        (m:specify-bar (car anchored-bars))
        #t)
       ((not (null? directioned-bars))
        (m:specify-bar (car directioned-bars))
        #t)
       (else
        (m:initialize-bar (car bars))
        #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Build ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:build-mechanism mechanism)
  (m:assemble-linkages (m:mechanism-bars mechanism)
                       (m:mechanism-joints mechanism))
  (let lp ()
    (run)
    (if (not (m:mechanism-fully-specified? mechanism))
        (if (m:specify-something mechanism)
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

#|
(let lp ()
  (initialize-scheduler)
  (let ((m (m:mechanism
            (m:establish-polygon-topology 'a 'b 'c 'd))))
    (pp (m:joint-anchored? (car (m:mechanism-joints m))))
    (m:build-mechanism m)
    (let ((f (m:mechanism->figure m)))
      (draw-figure f c)
      (pp (analyze-figure f)))))
|#
