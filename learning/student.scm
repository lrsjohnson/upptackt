;;; student.scm -- base model of a student's knowlege

;;; Commentary:

;; Ideas:
;; - Definitions, constructions, theorems
;; - "What is"

;; Future:
;; - Simplifiers of redudant / uninsteresting info
;; - Propose own investigations?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; Student Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <student>
  (%make-student definitions)
  student?
  (definitions student-definitions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-definition student def)
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;; Current Student ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *current-student* #f)

(define (student-lookup term)
  #f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Query ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (what-is term)
  (let ((result (student-lookup term)))
    (if (not result)
        'unknown
        result)))
