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

(define (make-student)
  (%make-student (make-key-weak-eq-hash-table)))


;;;;;;;;;;;;;;;;;;;;;;;; Building Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-predicate-for-definition s def)
  (let ((classification (definition-classification def))
        (restrictions (definition-restrictions def)))
   (let ((classification-predicate
          (definition-predicate (student-lookup s classification))))
     (lambda args
       (and (apply classification-predicate args)
            (every (lambda (r) (apply r args))
                   restrictions))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-definition! s def)
  (if (not (definition-predicate def))
      (set-definition-predicate!
       def
       (build-predicate-for-definition s def)))
  (hash-table/put! (student-definitions s)
                   (definition-name def)
                   def))

(define (lookup-definition s name)
  (hash-table/get (student-definitions s)
                  name
                  #f))

;;;;;;;;;;;;;;;;;;;;;;;;;; Current Student ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *current-student* #f)

(define (student-lookup s term)
  (lookup-definition s term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Query ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (what-is term)
  (let ((result (student-lookup *current-student* term)))
    (if (not result)
        'unknown
        result)))

(define (is-a? term obj)
  (let ((def (what-is term)))
    (if (not def)
        `(,term unknown)
        ((definition-predicate def) obj))))

(define (show-me term)
  (let ((def (what-is term)))
    (if (not def)
        `(,term unknown)
        (show-element ((definition-generator def))))))

(define (examine object)
  (filter (lambda (term)
            (is-a? term object))
          (hash-table/key-list (student-definitions *current-student*))))

;;;;;;;;;;;;;;;;;;;;;;;; Graphics Interfaces ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-element element)
  (draw-figure (figure element) c))

;;;;;;;;;;;;;;;;;;;;;;;; Initializing Student ;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-student)
  (let ((s (make-student)))
    (provide-core-knowledge s)
    (set! *current-student* s)))
