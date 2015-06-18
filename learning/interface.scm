;;; interface -- Main interface for learning module

;;; Discussion:

;; Ideas:
;; - "What is"

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Explanations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *explain* #f)

(define (with-explanation thunk)
  (fluid-let ((*explain* #t))
    (thunk)))


;;;;;;;;;;;;;;;;;;;;;;;;;; Current Student ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *current-student* #f)

(define (term-known? term)
  ((student-term-known? *current-student*) term))
(define (lookup term)
  ((student-lookup *current-student*) term))
(define (is-a? term obj)
  ((student-is-a? *current-student*) term obj))
(define (known-terms)
  ((student-known-terms *current-student*)))
(define (learn-term term object-generator)
  ((student-learn-term *current-student*) term object-generator))

(define (example-object term)
  ((definition-generator (lookup term))))

(define (more-specific? more-specific-term less-specific-term )
  (let ((more-specific-obj (example-object more-specific-term)))
    (is-a? less-specific-term more-specific-obj)))

(define less-specific? (flip-args more-specific?))

;;;;;;;;;;;;;;;;;;;;;;; Definitions Interface ;;;;;;;;;;;;;;;;;;;;;;;;

(define (what-is term)
  (if (not (term-known? term))
      (pprint 'unknown)
      (pprint (lookup term))))

(define (show-example term)
  (let ((def (lookup term)))
    (show-element ((definition-generator def)))))

(define (examine object)
  (let ((satisfying-terms
         (filter
          (lambda (term)
            (is-a? term object))
          (known-terms))))
    (remove-supplants more-specific? satisfying-terms)))

(define (show-definition-lattice)
  (show-lattice
   (student-lattice *current-student*)))

(define (show-definition-sublattice term)
  (show-lattice-from-key
   (student-lattice *current-student*)
   term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Applying ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-element element)
  (if (polygon? element)
      (name-polygon element))
  (let ((fig (figure (with-dependency '<premise> element))))
    (show-figure fig)
    (let ((obs-list (analyze-figure fig)))
      (map observation-with-premises obs-list))))

;;;;;;;;;;;;;;;;;;;;;;;; Graphics Interfaces ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-element element)
  (if (polygon? element)
      (name-polygon element))
  (show-figure (figure element)))

(define (show-figure figure)
  (draw-figure figure c))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Initial Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-student)
  (let ((s (make-student)))
    (set! *current-student* s)
    (provide-core-knowledge s)))
