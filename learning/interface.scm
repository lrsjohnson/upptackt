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
    (show-element more-specific-obj)
    (is-a? less-specific-term more-specific-obj)))

;;;;;;;;;;;;;;;;;;;;;;; Definitions Interface ;;;;;;;;;;;;;;;;;;;;;;;;

(define (what-is term)
  (if (not (term-known? term))
      (pprint 'unknown)
      (pprint (lookup term))))

(define (show-example term)
  (let ((def (lookup term)))
    (show-element ((definition-generator def)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Applying ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (examine object)
  (show-element object)
  (let ((applicable-terms
         (filter (lambda (term)
                   (internal-is-a? term object))
                 (all-known-terms))))
    applicable-terms))

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
    (provide-core-knowledge s)
    (set! *current-student* s)))
