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
        (conjectures (definition-conjectures def)))
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
             (every (lambda (o) (satisfies-conjecture o args))
                    conjectures))))))

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
  (or (lookup-definition s term)
      *unknown*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Query ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup term)
  (student-lookup *current-student* term))

(define *unknown* 'unknown)
(define (unknown? x)
  (eq? x *unknown*))

(define (what-is term)
  (pprint (lookup term)))

(define *explain* #f)

(define (is-a? term obj)
  (let ((def (lookup term)))
    (if (unknown? def)
        `(,term unknown)
        (fluid-let ((*explain* #t))
          ((definition-predicate def) obj)))))

(define (internal-is-a? term obj)
  (let ((def (lookup term)))
    (if (unknown? def)
        `(,term unknown)
        ((definition-predicate def) obj))))

(define (show-me term)
  (let ((def (lookup term)))
    (if (unknown? def)
        `(,term unknown)
        (show-element ((definition-generator def))))))

(define (examine object)
  (show-element object)
  (let ((applicable-terms
         (filter (lambda (term)
                   (internal-is-a? term object))
                 (all-known-terms))))
    applicable-terms))

(define (all-known-terms)
  (hash-table/key-list
   (student-definitions *current-student*)))

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
  (show-figure (figure element)))

(define (show-figure figure)
  (draw-figure figure c))

;;;;;;;;;;;;;;;;;;;;;;;;; Analyzing Elements ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-element element)
  (if (polygon? element)
      (name-polygon element))
  (let ((fig (figure element)))
    (show-figure fig)
    (analyze-figure fig)))

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
                 (base-conjectures (flatten (map definition-conjectures
                                                 base-definitions)))
                 (fig (figure (with-dependency '<premise> example)))
                 (observations (analyze-figure fig))
                 (conjectures (map conjecture-from-observation observations))
                 (simplified-conjectures
                  (simplify-conjectures conjectures base-conjectures)))
            (run-figure (lambda () (figure example)))
            (pprint conjectures)
            (let ((new-def
                   (make-restrictions-definition
                    term
                    simple-base-terms
                    simplified-conjectures
                    object-generator)))
              (add-definition! *current-student* new-def)
              'done))))))
