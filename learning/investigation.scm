;;; investigation.scm --- Investigation

;;; Code:

;;; Investigation Type

(define-record-type <investigation>
  (make-investigation starting-premise figure-proc)
  investigation?
  (starting-premise investigation-starting-premise)
  (figure-proc investigation-figure-procedure))


#|
Example:
|#

(define (diagonal-investigation)
  (make-investigation
   'equidiagonal
   (lambda (premise)
     (let-geo*
         ((((a b c d)) premise)
          (diag-1 (make-segment a c))
          (diag-2 (make-segment b d)))
       (figure premise diag-1 diag-2)))))

(define (midsegment-investigation)
  (make-investigation
   'quadrilateral
   (lambda (premise)
     (let-geo*
         ((((a b c d)) premise)
          (e (midpoint a b))
          (f (midpoint b c))
          (g (midpoint c d))
          (h (midpoint d a))
          (midsegment-1 (make-segment e g))
          (midsegment-2 (make-segment f h)))
       (figure premise midsegment-1 midsegment-2)))))

(define (run-investigation investigation)
  (let* ((starting-term
          (investigation-starting-premise investigation)))
    (for-each (lambda (descendent-term)
                (run-investigation-for-term
                 investigation descendent-term))
              (cons starting-term
                    (descendent-terms starting-term)))))

(define (run-investigation-for-term investigation premise-term)
  (pprint `(investigating ,premise-term))
  (let* ((figure-proc
          (investigation-figure-procedure investigation))
         (premise-def (lookup premise-term))
         (example (example-object premise-term)))
    (set-as-premise! example)
    (let* ((all-obs (all-observations (lambda () (figure-proc example))))
           (investigation-conjectures
            (map conjecture-from-observation all-obs))
           (orig-conjectures (all-conjectures-for-term premise-term))
           (new-conjectures (set-difference
                             investigation-conjectures
                             orig-conjectures
                             conjecture-equivalent?)))
      (set-definition-conjectures!
       premise-def
       (dedupe-by conjecture-equivalent?
                  (append orig-conjectures
                          investigation-conjectures)))
      (show-figure (figure-proc example))
      (remove-definition-lattice-node! premise-term)
      (add-definition-lattice-node! premise-term)
      (pprint new-conjectures))))
