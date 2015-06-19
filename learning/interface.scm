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

(define (lookup term)
  (or (lookup-definition term)
      (error "Term Unknown:" term)))

(define (example-object term)
  ((definition-generator (lookup term))))

(define (more-specific? more-specific-term less-specific-term )
  (let ((more-specific-obj (example-object more-specific-term)))
    (is-a? less-specific-term more-specific-obj)))

(define less-specific? (flip-args more-specific?))

(define (more-specific-nonrecursive?
         more-specific-term less-specific-term )
  (let ((more-specific-obj (example-object more-specific-term)))
    (is-a-nonrecursive? less-specific-term more-specific-obj)))

(define less-specific-nonrecursive?
  (flip-args more-specific-nonrecursive?))


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

(define (examine-primitive object)
  (let ((satisfying-terms
         (filter
          (lambda (term)
            (and (primitive-definition? (lookup term))
                 (is-a? term object)))
          (known-terms))))
    (remove-supplants more-specific? satisfying-terms)))

(define (show-definition-lattice)
  (show-lattice (definition-lattice)))

(define (show-definition-sublattice term)
  (show-lattice-from-key (definition-lattice) term))

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
    (provide-core-knowledge)))
