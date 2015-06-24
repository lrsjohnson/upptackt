
(define make-generic-operation make-generic-operator)
;;; Propagators also provide this. The above makes the below a
;;; compatible extension of that version
#|
;;;;           Most General Generic-Operator Dispatch
(declare (usual-integrations))		; for compiler

;;; Generic-operator dispatch is implemented here by a
;;; discrimination list (a "trie", invented by Ed Fredkin),
;;; where the arguments passed to the operator are examined
;;; by predicates that are supplied at the point of
;;; attachment of a handler.  (Handlers are attached by
;;; ASSIGN-OPERATION alias DEFHANDLER).

;;; The discrimination list has the following structure: it
;;; is an improper alist whose "keys" are the predicates
;;; that are applicable to the first argument.  If a
;;; predicate matches the first argument, the cdr of that
;;; alist entry is a discrimination list for handling the
;;; rest of the arguments.  Each discrimination list is
;;; improper: the cdr at the end of the backbone of the
;;; alist is the default handler to apply (all remaining
;;; arguments are implicitly accepted).

;;; A successful match of an argument continues the search
;;; on the next argument.  To be the correct handler all
;;; arguments must be accepted by the branch predicates, so
;;; this makes it necessary to backtrack to find another
;;; branch where the first argument is accepted if the
;;; second argument is rejected.  Here backtracking is
;;; implemented using #f as a failure return, requiring
;;; further search.

(define (make-generic-operator arity
                   #!optional name default-operation)
  (let ((record (make-operator-record arity)))

    (define (operator . arguments)
      (if (not (acceptable-arglist? arguments arity))
          (error:wrong-number-of-arguments
           (if (default-object? name) operator name)
           arity arguments))
      (apply (find-handler (operator-record-tree record)
                           arguments)
             arguments))

    (set-operator-record! operator record)

    (set! default-operation
      (if (default-object? default-operation)
          (named-lambda (no-handler . arguments)
            (error "Generic operator inapplicable:"
                   (if (default-object? name) operator name)
                   arguments))
          default-operation))
    (if (not (default-object? name))    ; Operation by name
        (set-operator-record! name record))

    (assign-operation operator default-operation)
    operator))

;;; This is the essence of the search.

(define (find-handler tree args)
  (if (null? args)
      tree
      (find-branch tree
		   (car args)
		   (lambda (result)
		     (find-handler result
				   (cdr args))))))

(define (find-branch tree arg next)
  (let loop ((tree tree))
    (cond ((pair? tree)
           (or (and ((caar tree) arg)
                    (next (cdar tree)))
               (loop (cdr tree))))
          ((null? tree) #f)
          (else tree))))

(define (assign-operation operator handler
                          . argument-predicates)
  (let ((record (get-operator-record operator))
        (arity (length argument-predicates)))
    (if record
        (begin
          (if (not (<= arity
                       (procedure-arity-min
                        (operator-record-arity record))))
              (error "Incorrect operator arity:" operator))
          (bind-in-tree argument-predicates
                        handler
                        (operator-record-tree record)
                        (lambda (new)
                          (set-operator-record-tree! record
                                                     new))))
        (error "Undefined generic operator" operator)))
  operator)

(define defhandler assign-operation)

(define (bind-in-tree keys handler tree replace!)
  (let loop ((keys keys) (tree tree) (replace! replace!))
    (cond ((pair? keys)   ; more argument-predicates
	   (let find-key ((tree* tree))
	     (if (pair? tree*)
		 (if (eq? (caar tree*) (car keys))
		     ;; There is already some discrimination
		     ;; list keyed by this predicate: adjust it
		     ;; according to the remaining keys
		     (loop (cdr keys)
			   (cdar tree*)
			   (lambda (new)
			     (set-cdr! (car tree*) new)))
		     (find-key (cdr tree*)))
		 (let ((better-tree
			(cons (cons (car keys) '()) tree)))
		   ;; There was no entry for the key I was
		   ;; looking for.  Create it at the head of
		   ;; the alist and try again.
		   (replace! better-tree)
		   (loop keys better-tree replace!)))))
	  ;; cond continues on next page.

	  ((pair? tree)  ; no more argument predicates.
            ;; There is more discrimination list here,
            ;; because my predicate list is a proper prefix
            ;; of the predicate list of some previous
            ;; assign-operation.  Insert the handler at the
            ;; end, causing it to implicitly accept any
            ;; arguments that fail all available tests.
	   (let ((p (last-pair tree)))
	     (if (not (null? (cdr p)))
		 (warn "Replacing a default handler:"
		       (cdr p) handler))
	     (set-cdr! p handler)))
	  (else
	   ;; There is no discrimination list here.  This
	   ;; handler becomes the discrimination list,
	   ;; accepting further arguments if any.
	   (if (not (null? tree))
	       (warn "Replacing a handler:" tree handler))
	   (replace! handler)))))

(define *generic-operator-table* (make-eq-hash-table))

(define (get-operator-record operator)
  (hash-table/get *generic-operator-table* operator #f))

(define (set-operator-record! operator record)
  (hash-table/put! *generic-operator-table* operator
                   record))

(define (make-operator-record arity) (cons arity '()))
(define (operator-record-arity record) (car record))
(define (operator-record-tree record) (cdr record))
(define (set-operator-record-tree! record tree)
  (set-cdr! record tree))

(define (acceptable-arglist? lst arity)
  (let ((len (length lst)))
    (and (fix:<= (procedure-arity-min arity) len)
         (or (not (procedure-arity-max arity))
             (fix:>= (procedure-arity-max arity) len)))))
|#
