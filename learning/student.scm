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
  (let ((classifications (definition-classifications def))
        (restrictions (definition-restrictions def)))
   (let ((classification-predicate
          (lambda (obj)
            (every
             (lambda (classification)
               (definition-predicate (student-lookup s classification)))
             classifications))))
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

(define *explain* #f)

(define (is-a? term obj)
  (show-element obj)
  (let ((def (what-is term)))
    (if (eq? def 'unknown)
        `(,term unknown)
        (fluid-let ((*explain* #t))
          ((definition-predicate def) obj)))))

(define (internal-is-a? term obj)
  (let ((def (what-is term)))
    (if (eq? def 'unknown)
        `(,term unknown)
        ((definition-predicate def) obj))))

(define (show-me term)
  (let ((def (what-is term)))
    (if (eq? def 'unknown)
        `(,term unknown)
        (show-element ((definition-generator def))))))

(define (examine object)
  (show-element object)
  (filter (lambda (term)
            (internal-is-a? term object))
          (hash-table/key-list (student-definitions *current-student*))))

;;;;;;;;;;;;;;;;;;;;;;;; Graphics Interfaces ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-element element)
  (draw-figure (figure element) c))

;;;;;;;;;;;;;;;;;;;;;;;; Initializing Student ;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-student)
  (let ((s (make-student)))
    (provide-core-knowledge s)
    (set! *current-student* s)))


(define (learn-term term object-generator)
  (let ((example (object-generator)))
    (let* ((base-terms (examine example))
          (fig (figure example))
          (results (analyze-figure fig)))
      (pprint results)
      (run-figure (lambda () (figure (object-generator))))
      (let ((new-def
             (make-restrictions-definition
              term
              base-terms
              (list (lambda (obj) (satisfies-observations results obj)))
              object-generator)))
        (add-definition! *current-student* new-def)))))
