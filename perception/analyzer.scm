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

(define (all-observations figure)
  (analyze figure))

(define (analyze-figure figure)
  (all-observations figure))

;;; Given a figure, report what's interesting
(define (all-observations figure)
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
     (extract-relationships points
                            (list concurrent-points-relationship
                                  concentric-relationship
                                  concentric-with-center-relationship))
     (extract-relationships segments
                             (list equal-length-relationship))
     (extract-relationships angles
                             (list equal-angle-relationship
                                   supplementary-angles-relationship
                                   complementary-angles-relationship))
     (extract-relationships linear-elements
                             (list parallel-relationship
                                   concurrent-relationship
                                   perpendicular-relationship
                                   )))))

(define (extract-relationships elements relationships)
  (append-map (lambda (r)
                (extract-relationship elements r))
              relationships))

(define (extract-relationship elements relationship)
  (map (lambda (tuple)
         (make-observation relationship tuple))
       (report-n-wise
        (relationship-arity relationship)
        (relationship-predicate relationship)
        elements)))

;;;;;;;;;;;;;;;;;;;;;; Interesting Observations ;;;;;;;;;;;;;;;;;;;;;;

(define (interesting-observations figure-proc)
  (set! *obvious-observations* '())
  (let ((all-obs (all-observations (figure-proc))))
    (pprint *obvious-observations*)
    (pprint all-obs)
    (set-difference all-obs *obvious-observations*
                    observation-equal?)))

(define *obvious-observations* #f)

(define (save-obvious-observation! obs)
  (if *obvious-observations*
      (begin
        (pprint obs)
        (set! *obvious-observations*
              (cons obs
                    *obvious-observations*)))))

;;;;;;;;;;;;;;;;;;;;;;; Cross products, pairs ;;;;;;;;;;;;;;;;;;;;;;;;

;;; General proceudres for generating pairs
(define (all-pairs elements)
  (all-n-tuples 2 elements))

(define (all-n-tuples n elements)
  (cond ((zero? n) '(()))
        ((< (length elements) n) '())
        (else
         (let lp ((elements-1 elements))
           (if (null? elements-1)
               '()
               (let ((element-1 (car elements-1))
                     (n-minus-1-tuples
                      (all-n-tuples (- n 1) (cdr elements-1))))
                 (append
                  (map
                   (lambda (rest-tuple)
                     (cons element-1 rest-tuple))
                   n-minus-1-tuples)
                  (lp (cdr elements-1)))))))))

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
(define ((nary-predicate n predicate) tuple)
  (apply predicate tuple))

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

(define (report-n-wise n predicate elements)
  (let ((tuples (all-n-tuples n elements)))
    (filter (nary-predicate n predicate) tuples)))

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
