;;; mechanism.scm --- Group of Bars / Joints

;;; Commentary:

;; Ideas:
;; - Grouping of bars and joints

;; Future:
;; - Also specify constraints with it
;; - Integrate with establishing toplogy

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Build ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:specify-bar bar)
  (let ((v (m:random-bar-length)))
    (pp `(specifying-bar ,(m:bar-name bar) ,v))
    (m:instantiate (m:bar-length bar) v 'specify-bar)))

(define (m:specify-joint joint)
  (let ((v (m:random-joint-theta)))
    (pp `(specifying-joint ,(m:joint-name joint) ,v))
    (m:instantiate (m:joint-theta joint) v 'specify-joint)))

(define (m:specify-something mechanism)
  ;; TODO: First try to specify a constraint
  (let ((bars (filter (notp m:bar-specified?)
                        (m:mechanism-bars mechanism)))
        (joints (filter (notp m:joint-specified?)
                          (m:mechanism-joints mechanism))))
    (let ((anchored-bars (filter m:bar-anchored? bars))
          (directioned-bars (filter m:bar-directioned? bars))
          (anchored-joints (filter m:joint-anchored? joints)))
      (cond
       ((not (null? anchored-bars))
        (m:specify-bar (car anchored-bars))
        #t)
       ((not (null? anchored-joints))
        (m:specify-joint (car anchored-joints))
        #t)
       ((not (null? directioned-bars))
        (m:specify-bar (car directioned-bars))
        #t)
       (else
        (m:initialize-bar (car bars))
        #t)))))

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
    (m:establish-polygon-topology 'a 'b 'c 'd))))
|#
