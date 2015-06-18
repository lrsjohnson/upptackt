;;; student.scm -- base model of a student's knowlege

;;; Commentary:

;; Ideas:
;; - Definitions, constructions, theorems

;; Future:
;; - Simplifiers of redudant / uninsteresting info
;; - Propose own investigations?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; Student Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <student>
  (%make-student definition-dictionary
                 definition-lattice)
  student?
  (definition-dictionary student-definitions)
  (definition-lattice student-lattice))

(define (make-student)
  (%make-student (make-key-weak-eq-hash-table)
                 (make-student-lattice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-definition! s def)
  (hash-table/put! (student-definitions s)
                   (definition-name def)
                   def))

(define (lookup-definition s name)
  (hash-table/get (student-definitions s)
                  name
                  #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lattice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-student-lattice)
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;; Student Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((student-lookup s) term)
  (or (lookup-definition s term)
      (error "Term Unknown:" term)))

(define ((student-term-known? s) term)
  (lookup-definition s term))

(define ((student-is-a? s) term obj)
  (let ((def ((student-lookup s) term)))
    (definition-holds? def obj)))

(define ((student-known-terms s))
  (hash-table/key-list
   (student-definitions s)))

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
             (every (lambda (o) (satisfies-conjecture? o args))
                    conjectures))))))

;;;;;;;;;;;;;;;;;;;;;;; Simplifying base terms ;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-base-terms terms)
  (let ((parent-terms (append-map
                       (lambda (t) (definition-classifications (lookup t)))
                           terms)))
    (filter (lambda (t) (not (memq t parent-terms)))
            terms)))
;;;;;;;;;;;;;;;;;;;;;;;; Initializing Student ;;;;;;;;;;;;;;;;;;;;;;;;

(define ((student-learn-term s) term object-generator)
  (if ((student-term-known? s) term)
      (error "Term already known:" term))
  (let ((example (name-polygon (object-generator))))
    (let* ((fig (figure (with-dependency '<premise example)))
           (observations (analyze-figure fig))
           (conjectures (map conjecture-from-observation observations)))
      (pprint conjectures)
      (let ((new-def
             (make-restrictions-definition
              term
              '()
              conjectures
              object-generator)))
        (if (not (definition-predicate new-def))
            (set-definition-predicate!
             new-def
             (build-predicate-for-definition s new-def)))
        (add-definition! s new-def)
        'done))))

(define (old-learn-term)
 (let* ((base-terms (examine example))
        (fig (figure (with-dependency '<premise> example)))
        (observations (analyze-figure fig))
        (conjectures (map conjecture-from-observation observations))
        (simplified-conjectures
         (simplify-conjectures conjectures base-conjectures)))
   (pprint conjectures)
   (let ((new-def
          (make-restrictions-definition
           term
           simple-base-terms
           simplified-conjectures
           object-generator)))
     (if (not (definition-predicate def))
         (set-definition-predicate!
          def
          (build-predicate-for-definition s def)))
     (add-definition! *current-student* new-def)
     'done)))

(define (get-simple-definitions term)
  (let ((def (lookup term)))
    (if (unknown? def)
        (error "Unknown term" term))
    (let* ((object ((definition-generator def)))
           (observations
            (filter
             observation->constraint
             (all-observations
              (figure (name-polygon object))))))
      (map
       (lambda (obs-subset)
         (pprint obs-subset)
         (let* ((topology (topology-for-object object))
                (new-figure
                 (observations->figure topology obs-subset)))
           (if new-figure
               (let ((new-polygon
                      (polygon-from-figure new-figure)))
                 (pprint new-polygon)
                 (if (is-a? term new-polygon)
                     (list 'valid-definition
                           obs-subset)
                     (list 'invalid-definition
                           obs-subset)))
               (list 'unknown-definition
                     obs-subset))))
       (all-subsets observations)))))
