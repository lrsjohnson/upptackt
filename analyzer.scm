;;; Analysis tools

(define (analyze-figure figure-proc)
  (analyze (figure-proc)))


(define (results-with-names result-type elements)
  (map (lambda (p-pair)
         (cons result-type (map element-name p-pair)))
       elements))

;;; Given a figure, report what's interesting
(define (analyze figure)
  (let* ((points (figure-filter point? figure))
         (angles (figure-filter angle? figure)))
    (let ((concurrent-points (report-concurrent-points points))
          (equal-angles (report-equal-angles angles)))
      (append
       (results-with-names 'concurrent concurrent-points)
       (results-with-names 'angle-equal equal-angles)))))

;;; General proceudres for generating pairs
(define (all-pairs elements)
  (let lp-1 ((elements-1 elements))
    (if (null? elements-1)
        '()
        (let ((el-1 (car elements-1))
              (rest-1 (cdr elements-1)))
          (append (let lp-2 ((elements-2 rest-1))
                    (if (null? elements-2)
                        '()
                        (let ((el-2 (car elements-2))
                              (rest-2 (cdr elements-2)))
                          (cons (list el-1 el-2)
                                (lp-2 rest-2)))))
                  (lp-1 rest-1))))))

;;; Check for pairwise equality
(define ((pair-predicate predicate) pair)
  (predicate (car pair) (cadr pair)))

(define ((report-pairwise predicate) elements)
  (let ((elt-pairs (all-pairs elements)))
    (filter (pair-predicate predicate) elt-pairs)))

;;; Check for concurrent points
(define (report-concurrent-points points)
  ((report-pairwise point-equal?) points))

;;; Check for equal angles
(define (report-equal-angles angles)
  ((report-pairwise angle-measure-equal?) angles))
