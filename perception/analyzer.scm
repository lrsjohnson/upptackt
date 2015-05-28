;;; analyzer.scm --- Tools for analyzing Diagram

;;; Commentary

;; Ideas:
;; - Analyze figrue to dermine properties "beyond coincidence"
;; - Use dependency structure to eliminate some obvious examples.

;; Future:
;; - Add More "interesting properties"
;; - Create storage for learned properties.
;; - Output format, add names
;; - Separate "discovered" from old properties.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;; Main Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-figure figure)
  (analyze figure))

;;; Given a figure, report what's interesting
(define (analyze figure)
  (number-figure-random-dependencies! figure)
  (let* ((points (figure-points figure))
         (angles (figure-angles figure))
         (implied-segments '() ; (point-pairs->segments (all-pairs points))
                           )
         (linear-elements (append
                           (figure-linear-elements figure)
                           implied-segments))
         (segments (append (figure-segments figure)
                           implied-segments)))
    (append
     (pairwise-relationships segments
                             (list equal-length-relationship))
     (pairwise-relationships angles
                             (list equal-angle-relationship
                                   supplementary-angles-relationship
                                   complementary-angles-relationship))
     (pairwise-relationships linear-elements
                             (list parallel-relationship
                                   perpendicular-relationship)))))

(define (pairwise-relationships elements relationships)
  (append-map (lambda (r)
                (pairwise-relationship elements r))
              relationships))

(define (pairwise-relationship elements relationship)
  (map (lambda (pair)
         (make-observation '() relationship pair))
       (report-pairwise (relationship-predicate relationship)
                        elements)))

;;;;;;;;;;;;;;;;;;;;;;; Cross products, pairs ;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;; Obvious Segments ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (segment-for-endpoint p1)
  (let ((dep (element-dependency p1)))
    (and dep
         (or (and (eq? (car dep) 'segment-endpoint-1)
                  (cadr dep))
             (and (eq? (car dep) 'segment-endpoint-2)
                  (cadr dep))))))

(define (derived-from-same-segment? p1 p2)
  (and
   (segment-for-endpoint p1)
   (segment-for-endpoint p2)
   (eq? (segment-for-endpoint p1)
        (segment-for-endpoint p2))))

(define (polygon-for-point p1)
  (let ((dep (element-dependency p1)))
    (and dep
         (and (eq? (car dep) 'polygon-point)
              (cons (caddr dep)
                    (cadr dep))))))

(define (adjacent-in-same-polygon? p1 p2)
  (let ((poly1 (polygon-for-point p1))
        (poly2 (polygon-for-point p2)))
    (and poly1 poly2
         (eq? (car poly1) (car poly2))
         (or (= (abs (- (cdr poly1)
                        (cdr poly2)))
                1)
             (and (= (cdr poly1) 0)
                  (= (cdr poly2) 3))
             (and (= (cdr poly1) 3)
                  (= (cdr poly2) 0))))))

(define (point-pairs->segments ppairs)
  (filter (lambda (segment) segment)
          (map (lambda (point-pair)
                 (let ((p1 (car point-pair))
                       (p2 (cadr point-pair)))
                   (and (not (point-equal? p1 p2))
                        (not (derived-from-same-segment? p1 p2))
                        (not (adjacent-in-same-polygon? p1 p2))
                        (make-auxiliary-segment
                         (car point-pair)
                         (cadr point-pair))))) ; TODO: Name segment
               ppairs)))

;;;;;;;;;;;;;;;;;;;;;;;;; Dealing with pairs ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check for pairwise equality
(define ((pair-predicate predicate) pair)
  (predicate (car pair) (cadr pair)))

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

(define (report-pairwise predicate elements)
  (let ((elt-pairs (all-pairs elements)))
    (filter (pair-predicate predicate) elt-pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Results: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-analysis-collector)
  (make-equal-hash-table))

(define (save-results results data-table)
  (hash-table/put! data-table results
                   (+ 1 (hash-table/get data-table results 0))))

(define (print-analysis-results data-table)
  (hash-table/for-each
   data-table
   (lambda (k v)
     (pprint (list v (cons 'discovered k))))))
