;;; close-enuf? floating point comparison from scmutils
;;; Origin: Gerald Jay Sussman

(define *machine-epsilon*
  (let loop ((e 1.0))
    (if (= 1.0 (+ e 1.0))
        (* 2 e)
        (loop (/ e 2)))))

(define *sqrt-machine-epsilon*
  (sqrt *machine-epsilon*))

#|
 (define (close-enuf? h1 h2 tolerance)
   (<= (magnitude (- h1 h2))
       (* .5 (max tolerance *machine-epsilon*)
          (+ (magnitude h1) (magnitude h2) 2.0))))
|#

(define (close-enuf? h1 h2 #!optional tolerance scale)
  (if (default-object? tolerance)
      (set! tolerance (* 10 *machine-epsilon*)))
  (if (default-object? scale)
      (set! scale 1.0))
  (<= (magnitude (- h1 h2))
      (* tolerance
         (+ (* 0.5
               (+ (magnitude h1) (magnitude h2)))
            scale))))

(define (assert boolean error-message)
  (if (not boolean) (error error-message)))

(define (flatten list)
  (cond ((null? list) '())
        ((list? (car list))
         (append (flatten (car list))
                 (flatten (cdr list))))
        (else (cons (car list) (flatten (cdr list))))))

(define ((notp predicate) x)
  (not (predicate x)))

(define ((andp p1 p2) x)
  (and (p1 x)
       (p2 x)))

(define (true-proc . args) #t)
(define (false-proc . args) #f)

(define (identity x) x)

;;; ps1 \ ps2
(define (set-difference set1 set2 member-predicate)
  (define delp (delete-member-procedure list-deletor member-predicate))
  (let lp ((set1 set1)
           (set2 set2))
    (if (null? set2)
        set1
        (let ((e (car set2)))
          (lp (delp e set1)
              (cdr set2))))))

(define (eq-append! element key val)
  (eq-put! element key
           (cons val
                 (or (eq-get element key) '()))))

(define (sort-by-key l key)
  (sort l (lambda (v1 v2)
            (< (key v1)
               (key  v2)))))

(define (sort-by-key-2 l key)
  (let ((v (sort-by-key-2 l key)))
    (pprint (map (lambda (x) (cons (name x) (key x))) v))
    v))

(define ((negatep f) x)
  (- (f x)))
