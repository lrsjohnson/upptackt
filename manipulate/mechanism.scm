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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Build ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:build-mechanism mechanism)
  (m:assemble-linkages (m:mechanism-bars mechanism)
                       (m:mechanism-joints mechanism)))
