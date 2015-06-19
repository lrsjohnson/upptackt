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
   'rhombus
   (lambda (rhombus)
     (let-geo*
         ((((a b c d)) rhombus)
          (diag-1 (make-segment a c))
          (diag-2 (make-segment b d)))
       (figure rhombus diag-1 diag-2)))))

(define (run-investigation investigation)
  (let* ((premise-term (investigation-premise investigation))
         (figure-proc
          (investigation-figure-procedure investigation))
         (example (example-object premise-term)))
    (set-as-premise! example)
    (let* ((all-obs (all-observations (lambda () (figure-proc example))))
           (conjectures (map conjecture-from-observation all-obs))
           (orig-conjectures (all-conjectures-for-term premise-term)))
      (show-figure (figure-proc example))
      (pprint (set-difference
               conjectures
               orig-conjectures
               conjecture-equivalent?)))))
