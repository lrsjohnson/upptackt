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
        (observations (definition-observations def)))
    (let ((classification-predicate
           (lambda (obj)
             (every
              (lambda (classification)
                (or ((definition-predicate (student-lookup s classification))
                     obj)
                    (begin (if *explain*
                               (pprint `(failed-classification
                                         ,classification)))
                           #f)))
              classifications))))
      (lambda args
        (and (apply classification-predicate args)
             (every (lambda (o) (satisfies-observation o args))
                    observations))))))

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

(define (lookup term)
  (let ((result (student-lookup *current-student* term)))
    (if (not result)
        'unknown
        result)))

(define (what-is term)
  (pprint (lookup term)))

(define *explain* #f)

(define (is-a? term obj)
  (show-element obj)
  (let ((def (lookup term)))
    (if (eq? def 'unknown)
        `(,term unknown)
        (fluid-let ((*explain* #t))
          ((definition-predicate def) obj)))))

(define (internal-is-a? term obj)
  (let ((def (lookup term)))
    (if (eq? def 'unknown)
        `(,term unknown)
        ((definition-predicate def) obj))))

(define (show-me term)
  (let ((def (lookup term)))
    (if (eq? def 'unknown)
        `(,term unknown)
        (show-element ((definition-generator def))))))

(define (examine object)
  (show-element object)
  (let ((base-terms (filter (lambda (term)
                              (internal-is-a? term object))
                            (hash-table/key-list
                             (student-definitions *current-student*)))))
    base-terms))

;;;;;;;;;;;;;;;;;;;;;;; Simplifying base terms ;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-base-terms terms)
  (let ((parent-terms (append-map
                       (lambda (t) (definition-classifications (lookup t)))
                           terms)))
    (filter (lambda (t) (not (memq t parent-terms)))
            terms)))

;;;;;;;;;;;;;;;;;;;;;;;; Graphics Interfaces ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-element element)
  (if (polygon? element)
      (name-polygon element))
  (draw-figure (figure element) c))

;;;;;;;;;;;;;;;;;;;;;;;; Initializing Student ;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-student)
  (let ((s (make-student)))
    (provide-core-knowledge s)
    (set! *current-student* s)))


(define (learn-term term object-generator)
  (let ((v (lookup term)))
    (if (not (eq? v 'unknown))
        (pprint `(already-known ,term))
        (let ((example (name-polygon (object-generator))))
          (let* ((base-terms (examine example))
                 (simple-base-terms (simplify-base-terms base-terms))
                 (base-definitions (map lookup base-terms))
                 (base-observations (flatten (map definition-observations
                                                  base-definitions)))
                 (fig (figure (with-dependency '<premise> example)))
                 (observations (analyze-figure fig))
                 (simplified-observations
                  (simplify-observations observations base-observations)))
            (run-figure (lambda () (figure (object-generator))))
            (pprint observations)
            (let ((new-def
                   (make-restrictions-definition
                    term
                    simple-base-terms
                    simplified-observations
                    object-generator)))
              (add-definition! *current-student* new-def)
              'done))))))
