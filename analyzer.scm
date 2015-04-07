;;; Analysis tools

(define (analyze-figure figure)
  (analyze figure))


(define (results-with-names result-type elements)
  (map (lambda (p-pair)
         (cons result-type (map element-name p-pair)))
       elements))

;;; Given a figure, report what's interesting
(define (analyze figure)
  (let* ((points (append (figure-filter point? figure)
                         (append-map (lambda (polygon) (polygon-points polygon))
                                     (figure-filter polygon? figure))))
         (angles (figure-filter angle? figure))
         (linear-elements (figure-filter linear-element? figure))
         (implied-segments (point-pairs->segments (all-pairs points))))
    (append (results-with-names 'concurrent (report-concurrent-points points))
            (results-with-names 'angle-equal (report-equal-angles angles))
            (results-with-names 'supplementary (report-supplementary-angles angles))
            (results-with-names 'complementary (report-complementary-angles angles))
            (results-with-names 'parallel (report-parallel-elements linear-elements))
            (results-with-names 'perpendicular (report-perpendicular-elements linear-elements))
            (results-with-names 'equal-length (report-equal-segments implied-segments)))))

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

;;; Obtaining segments

(define (point-pairs->segments ppairs)
  (filter (lambda (segment) segment)
          (map (lambda (point-pair)
                 (let ((p1 (car point-pair))
                       (p2 (cadr point-pair)))
                   (and (not (point-equal? p1 p2))
                        (make-segment (car point-pair)
                                      (cadr point-pair))))) ; TODO: Name segment
               ppairs)))

;;; Check for pairwise equality
(define ((pair-predicate predicate) pair)
  (predicate (car pair) (cadr pair)))

(define (hash-table/append table key element)
  (hash-table/put! table
                   key
                   (cons element
                         (hash-table/get table key '()))))

;;; Merges "connected-components" of pairs
(define (merge-pair-groups elements pairs)
  (let ((i 0)
        (group-ids (make-key-weak-eq-hash-table))
        (group-elements (make-key-weak-eq-hash-table))) ; Map from pair
    (for-each (lambda (pair)
                (let ((first (car pair))
                      (second (cadr pair)))
                  (let ((group-id-1 (hash-table/get group-ids first i))
                        (group-id-2 (hash-table/get group-ids second i)))
                    (cond ((and (= group-id-1 i)
                                (= group-id-2 i))
                           ;; Both new, new groups:
                           (hash-table/put! group-ids first group-id-1)
                           (hash-table/put! group-ids second group-id-1))
                          ((= group-id-1 i)
                           (hash-table/put! group-ids first group-id-2))
                          ((= group-id-2 i)
                           (hash-table/put! group-ids second group-id-1)))
                    (set! i (+ i 1)))))
              pairs)
    (for-each (lambda (elt)
                (hash-table/append group-elements
                                   (hash-table/get group-ids elt 'invalid)
                                   elt))
              elements)
    (hash-table/remove! group-elements 'invalid)
    (hash-table/datum-list group-elements)))

(define ((report-pairwise predicate) elements)
  (let ((elt-pairs (all-pairs elements)))
    (merge-pair-groups elements (filter (pair-predicate predicate) elt-pairs))))

;;; Check for concurrent points
(define (report-concurrent-points points)
  ((report-pairwise point-equal?) points))

;;; Check for equal angles
(define (report-equal-angles angles)
  ((report-pairwise angle-measure-equal?) angles))

;;; Check for supplementary angles
(define (report-supplementary-angles angles)
  ((report-pairwise supplementary-angles?) angles))

;;; Check for complementary angles
(define (report-complementary-angles angles)
  ((report-pairwise complementary-angles?) angles))

;;; Check for parallel lines and segments
(define (report-parallel-elements linear-elements)
  ((report-pairwise parallel?) linear-elements))

;;; Check for parallel lines and segments
(define (report-perpendicular-elements linear-elements)
  ((report-pairwise perpendicular?) linear-elements))

;;; Check for parallel lines and segments
(define (report-equal-segments implied-segments)
  ((report-pairwise segment-equal-length?) implied-segments))

;;; Results:

(define (make-analysis-collector)
  (make-equal-hash-table))


(define (save-results results data-table)
  (hash-table/put! data-table results
                   (+ 1 (hash-table/get data-table results 0))))

(define (print-analysis-results data-table)
  (hash-table/for-each
   data-table
   (lambda (k v)
     (pp (list v (cons 'discovered k))))))
