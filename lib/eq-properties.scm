;;;; Traditional LISP property lists
;;; extended to work on any kind of eq? data structure.

(declare (usual-integrations))

;;; Property lists are a way of creating data that looks like a record
;;; structure without commiting to the fields that will be used until
;;; run time.  The use of such flexible structures is frowned upon by
;;; most computer scientists, because it is hard to statically
;;; determine the bounds of the behavior of a program written using
;;; this stuff.  But it makes it easy to write programs that confuse
;;; such computer scientists.  I personally find it difficult to write
;;; without such crutches.  -- GJS


(define eq-properties (make-eq-hash-table))

(define (eq-put! node property value)
  (let ((plist (hash-table/get eq-properties node #f)))
    (if plist
	(let ((vcell (assq property (cdr plist))))
	  (if vcell
	      (set-cdr! vcell value)
	      (set-cdr! plist
			(cons (cons property value)
			      (cdr plist)))))
	(hash-table/put! eq-properties node
			 (list node (cons property value)))))
  'done)

(define (eq-adjoin! node property new)
  (eq-put! node property
	   (eq-set/adjoin new
			  (or (eq-get node property) '())))
  'done)

(define (eq-rem! node property)
  (let ((plist (hash-table/get eq-properties node #f)))
    (if plist
	(let ((vcell (assq property (cdr plist))))
	  (if vcell
	      (hash-table/put! eq-properties node (delq! vcell plist))))))
  'done)


(define (eq-get node property)
  (let ((plist (hash-table/get eq-properties node #f)))
    (if plist
	(let ((vcell (assq property (cdr plist))))
	  (if vcell
	      (cdr vcell)
	      #f))
	#f)))

(define (eq-plist node)
  (hash-table/get eq-properties node #f))


(define (eq-path path)
  (define (lp node)
    (if node
	(if (pair? path)
	    (eq-get ((eq-path (cdr path)) node)
		    (car path))
	    node)
	#f))
  lp)
