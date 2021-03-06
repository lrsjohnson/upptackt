(define (assert boolean error-message)
  (if (not boolean) (error error-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; List Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sort-by-key l key)
  (sort l (lambda (v1 v2)
            (< (key v1)
               (key  v2)))))

(define (index-of el list equality-predicate)
  (let lp ((i 0)
           (l list))
    (cond ((null? l) #f)
          ((equality-predicate (car l) el)
           i)
          (else (lp (+ i 1) (cdr l))))))

;;; Swaps the elements at indices i and j in the vector
(define (swap vec i j)
  (let ((tmp (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (shuffle alts)
  (let ((alts-vec (list->vector alts))
        (num-alts (length alts)))
    (if (= num-alts 0)
        alts
        (let lp ((to-index (- num-alts 1)))
          (cond
           ((= to-index 0) (vector->list alts-vec))
           (else (let ((from-index
                        (random (+ 1 to-index))))
                   (swap alts-vec from-index to-index)
                   (lp (- to-index 1)))))))))

(define (flatten list)
  (cond ((null? list) '())
        ((list? (car list))
         (append (flatten (car list))
                 (flatten (cdr list))))
        (else (cons (car list) (flatten (cdr list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((notp predicate) x)
  (not (predicate x)))

(define ((andp p1 p2) x)
  (and (p1 x)
       (p2 x)))

(define (true-proc . args) #t)
(define (false-proc . args) #f)

(define (identity x) x)

(define (true? x)
  (if x #t #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Set Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ps1 \ ps2
(define (set-difference set1 set2 equality-predicate)
  (define delp (delete-member-procedure list-deletor equality-predicate))
  (let lp ((set1 set1)
           (set2 set2))
    (if (null? set2)
        (dedupe-by equality-predicate set1)
        (let ((e (car set2)))
          (lp (delp e set1)
              (cdr set2))))))

(define (subset? small-set big-set equality-predicate)
  (let ((sd (set-difference small-set big-set equality-predicate)))
    (null? sd)))

(define (set-equivalent? set1 set2 equality-predicate)
  (and (subset? set1 set2 equality-predicate)
       (subset? set2 set1 equality-predicate)))

(define (set-equivalent-procedure equality-predicate)
  (lambda (set1 set2)
    (set-equivalent? set1 set2 equality-predicate)))

(define (eq-subset? small-set big-set)
  (subset? small-set big-set eq?))

(define (set-intersection set1 set2 member-predicate)
  (let lp ((set1 (dedupe member-predicate set1))
           (intersection '()))
    (if (null? set1)
        intersection
        (let ((e (car set1)))
          (lp (cdr set1)
              (if (member-predicate e set2)
                  (cons e intersection)
                  intersection))))))

(define (distinct? elements equality-predicate)
  (= (length elements)
     (length (set-intersection
              elements elements
              (member-procedure equality-predicate)))))

(define (dedupe-eq elements)
  (dedupe-by eq? elements))

(define (dedupe-by equality-predicate elements)
  (dedupe (member-procedure equality-predicate) elements))

(define (dedupe member-predicate elements)
  (cond ((null? elements) '())
        (else
         (let ((b1 (car elements)))
           (if (member-predicate b1 (cdr elements))
               (dedupe member-predicate (cdr elements))
               (cons b1 (dedupe member-predicate (cdr elements))))))))

;;; supplanted-by-prediate takes two args: an element under consideration
;;; and an existing element in the list. If true, the first element
;;; will be removed from the list.
(define (remove-supplanted supplants-predicate elements)
  (define member-predicate (member-procedure
                            supplants-predicate))
  (let lp ((elements-tail elements)
           (elements-head '()))
    (if (null? elements-tail)
        elements-head
        (let ((el (car elements-tail))
              (new-tail (cdr elements-tail)))
          (lp new-tail
              (if (or (member-predicate el new-tail)
                      (member-predicate el elements-head))
                  elements-head
                  (cons el elements-head)))))))

(define (all-subsets elements)
  (append-map
   (lambda (n)
     (all-n-tuples n elements))
   (iota (+ (length elements) 1))))

;;;;;;;;;;;;;;;;;;;;;;;; Equivalence Classes ;;;;;;;;;;;;;;;;;;;;;;;;;

(define (partition-into-equivalence-classes elements equivalence-predicate)
  (let lp ((equivalence-classes '())
           (remaining-elements elements))
    (if (null? remaining-elements)
        equivalence-classes
        (lp
         (add-to-equivalence-classes
          equivalence-classes
          (car remaining-elements)
          (member-procedure equivalence-predicate))
         (cdr remaining-elements)))))

(define (add-to-equivalence-classes classes element memp)
  (if (null? classes)
      (list (list element))
      (let ((first-class (car classes))
            (remaining-classes (cdr classes)))
        (if (memp element first-class)
            (cons (cons element first-class)
                  remaining-classes)
            (cons first-class
                  (add-to-equivalence-classes remaining-classes
                                              element
                                              memp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Majorities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Runs procedure on random animation frames and checks that results
;;; appear in a majority of frames.

(define *majority-trials-total* 3)
(define *majority-trials-required* 2)

(define (require-majority f equality-predicate)
  (require-enough f *majority-trials-total* *majority-trials-required*
                  equality-predicate))

(define (require-enough f total-trials num-required equality-predicate)
  (let ((all-executions (n-random-frames total-trials f)))
    (check-enough all-executions num-required equality-predicate)))

(define (check-enough execution-results num-required equality-predicate)
  (let ((hash-table ((weak-hash-table/constructor
                      (lambda (a b) 1) equality-predicate))))
    (for-each (lambda (execution-result)
                (for-each (lambda (element)
                            (hash-table/append hash-table
                             element element))
                          execution-result))
              execution-results)
    (filter identity
            (map (lambda (a-pair)
                   (and (>= (length (cdr a-pair)) num-required)
                        (car a-pair)))
                 (hash-table->alist hash-table)))))

;;;;;;;;;;;;;;;;;;;;;;;;; Function Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((negatep f) x)
  (- (f x)))

(define ((flip-args f) x y)
  (f y x))

(define (memoize-function f)
  (let ((cache (make-key-weak-eq-hash-table)))
    (lambda (arg)
      (hash-table/intern!
       cache
       arg
       (lambda () (f arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Other Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eq-append! element key val)
  (eq-put! element key
           (cons val
                 (or (eq-get element key) '()))))

;;; (nth-letter-symbol 1) => 'a , 2 => 'b, etc.
(define (nth-letter-symbol i)
  (symbol (make-char (+ 96 i) 0)))

(define (hash-table/append table key element)
  (hash-table/put! table
                   key
                   (cons element
                         (hash-table/get table key '()))))
