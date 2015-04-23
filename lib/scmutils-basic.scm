;;; Procedures copied from scmutils to support multimin.scm


(define generate-list make-initialized-list)

;;; generic.scm (basic)
(define g:* *)
(define g:+ +)
(define g:- -)


;;; vector.scm
(define (v:make-basis-unit n i)	; #(0 0 ... 1 ... 0) n long, 1 in ith position
  (v:generate n (lambda (j) (if (fix:= j i) 1 0))))

(define-integrable v:generate make-initialized-vector)

(define (scalar*vector s v)
  (v:generate (vector-length v)
    (lambda (i)
      (g:* s (vector-ref v i)))))

(define (vector+vector v1 v2)
  (v:generate (vector-length v1)
    (lambda (i)
      (g:+ (vector-ref v1 i)
	   (vector-ref v2 i)))))

(define (vector-vector v1 v2)
  (v:generate (vector-length v1)
    (lambda (i)
      (g:- (vector-ref v1 i)
	   (vector-ref v2 i)))))

;;; utils.scm
(define (a-reduce f l)
  (define (loop l)
     (if (null? (cdr l))
         (car l)
         (loop (cons (f (car l) (cadr l)) (cddr l)))))
  (if (null? l)
      (error "Reduce no elements")
      (loop l)))
