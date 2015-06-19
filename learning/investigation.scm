;;; investigation.scm --- Investigation

;;; Code:

;;; Investigation Type

(define-record-type <investigation>
  (make-investigation premise-term figure-proc)
  investigation?
  (premise-term investigation-premise)
  (figure-proc investigation-figure-procedure))


#|
Example:
|#

(define (diagonals-investigation)
  (make-investigation
   'orthodiagonal
   (lambda (premise)
     (let-geo*
         ((((a b c d)) premise)
          (diag-1 (make-segment a c))
          (diag-2 (make-segment b d)))
       (figure premise diag-1 diag-2)))))

(define (run-investigation investigation)
  (let* ((premise-term (investigation-premise investigation))
         (premise-def (lookup premise-term))
         (figure-proc
          (investigation-figure-procedure investigation))
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
       (dedupe-by conjecture-equivalent? (append orig-conjectures
                                                 investigation-conjectures)))
      (show-figure (figure-proc example))
      (remove-definition-lattice-node! premise-term)
      (add-definition-lattice-node! premise-term)
      (pprint new-conjectures))))
