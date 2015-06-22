;;; student.scm -- base model of a student's knowlege

;;; Commentary:

;; Ideas:
;; - Definitions, constructions, theorems

;; Future:
;; - Simplifiers of redudant / uninsteresting info
;; - Propose own investigations?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; Student Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <student>
  (%make-student definition-dictionary
                 definition-lattice)
  student?
  (definition-dictionary student-definitions)
  (definition-lattice student-definition-lattice))

(define (make-student)
  (%make-student (make-key-weak-eq-hash-table)
                 (make-student-lattice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lattice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-student-lattice)
  (make-lattice less-specific-nonrecursive?
                (make-lattice-node 'object 'object)))

;;;;;;;;;;;;;;;;; Procedures using student directly ;;;;;;;;;;;;;;;;;;

(define (student-lookup-definition s name)
  (hash-table/get (student-definitions s) name #f))

(define (student-save-definition! s def)
  (hash-table/put! (student-definitions s)
                   (definition-name def)
                   def))

(define (student-known-terms s)
  (hash-table/key-list
   (student-definitions s)))

;;;;;;;;;;;;;;;;;;;; Public Versionns of student ;;;;;;;;;;;;;;;;;;;;;

(define *current-student* #f)

(define (lookup-definition term)
  (student-lookup-definition *current-student* term))

(define (save-definition! def)
  (student-save-definition! *current-student* def))

(define (definition-lattice)
  (student-definition-lattice *current-student*))

(define (known-terms)
  (student-known-terms *current-student*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-definition-lattice-node! term)
  (add-lattice-node
   (definition-lattice) (make-lattice-node term term))
  (update-definitions-from-lattice
   (cons term (child-terms term))))

(define (remove-definition-lattice-node! term)
  (let ((old-parent-terms (parent-terms term))
        (old-child-terms (child-terms term)))
    (remove-lattice-node
     (definition-lattice) term)
    (update-definitions-from-lattice old-parent-terms)
    (update-definitions-from-lattice old-child-terms)))

(define (add-definition! def)
  (let ((term (definition-name def)))
    (if (lookup-definition name)
        (error "Definition already exists for" term))
    (save-definition! def)
    (add-definition-lattice-node! term)))

;;;;;;;;;;;;;;;;;;;;;;;;; Student Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (term-known? term)
  (lookup-definition term))

(define (is-a? term obj)
  (let ((def (lookup term)))
    (definition-holds? def obj)))

(define (is-a-nonrecursive? term obj)
  (let ((def (lookup term)))
    (definition-holds-nonrecursive? def obj)))

;;;;;;;;;;;;;;;;;;;;;;;; Learning Terms ;;;;;;;;;;;;;;;;;;;;;;;;

(define (learn-term term object-generator)
  (if (term-known? term)
      (error "Term already known:" term))
  (let ((example (name-polygon (object-generator))))
    (let* ((primitive-predicate (get-primitive-predicate example))
           (fig (figure (as-premise example 0)))
           (observations (analyze-figure fig))
           (conjectures (map conjecture-from-observation observations)))
      (pprint conjectures)
      (let ((new-def
             (make-definition term object-generator
                primitive-predicate conjectures)))
        (add-definition! new-def)
        (check-new-def new-def)
        'done))))

(define (get-primitive-predicate object)
  (let ((primitives (examine-primitive object)))
    (definition-predicate (lookup (car primitives)))))

(define (check-new-def new-def)
  (if (and (= 1 (length (definition-classifications new-def)))
           (null? (definition-specific-conjectures new-def)))
      (pp (string-append
           "Warning: No new known properties for term: "
           (symbol->string (definition-name new-def))
           ". Appears same as "
           (symbol->string (car (definition-classifications new-def)))))))

(define (all-conjectures-for-term term)
  (let* ((ancestor-terms (ancestor-terms term))
         (ancestor-defs (map lookup ancestor-terms))
         (ancestor-conjectures
          (append-map definition-specific-conjectures ancestor-defs)))
    (append (definition-specific-conjectures (lookup term))
            ancestor-conjectures)))

(define (update-definitions-from-lattice terms)
  (for-each update-definition-from-lattice terms))

(define (update-definition-from-lattice term)
  (let* ((def (lookup term))
         (current-conjectures (definition-conjectures def))
         (parent-terms (parent-terms term))
         (ancestor-terms (ancestor-terms term))
         (ancestor-defs (map lookup ancestor-terms))
         (ancestor-conjectures
          (append-map definition-conjectures ancestor-defs))
         (new-conjectures
          (set-difference current-conjectures
                          ancestor-conjectures
                          conjecture-equal?)))
    (set-definition-classifications!
     def
     parent-terms)
    (set-definition-specific-conjectures!
     def
     new-conjectures)))

(define (lattice-node-for-term term)
  (lattice-node-by-key (definition-lattice) term))

(define (child-terms term)
  (let* ((lattice-node (lattice-node-for-term term))
         (child-nodes (lattice-node-children lattice-node)))
    (map lattice-node-key child-nodes)))

(define (parent-terms term)
  (let* ((lattice-node (lattice-node-for-term term))
         (parent-nodes (lattice-node-parents lattice-node)))
    (map lattice-node-key parent-nodes)))

(define (ancestor-terms term)
  (let ((ancestor-nodes (sublattice-nodes-upwards
                         (definition-lattice)
                         term)))
    (delq term (map lattice-node-key ancestor-nodes))))

(define (descendent-terms term)
  (let ((descendent-nodes (sublattice-nodes
                           (definition-lattice)
                           term)))
    (delq term (map lattice-node-key descendent-nodes))))

;;;;;;;;;;;;;;;;;;;; Getting Implied Observations ;;;;;;;;;;;;;;;;;;;;

(define (observations-implied-by-term term object)
  (let ((conjectures (all-conjectures-for-term term)))
    (map (lambda (conjecture)
           (observation-from-conjecture conjecture (list object)))
         conjectures)))

;;;;;;;;;;;;;;;;;;;;; Performing Investigations ;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;; Simplifying Definitions ;;;;;;;;;;;;;;;;;;;;;;;

(define (polygon-from-object-observations object obs-subset)
  (let* ((topology (topology-for-object object))
         (new-figure (observations->figure topology obs-subset)))
    (and new-figure (object-from-new-figure object new-figure))))

(define (get-simple-definitions term)
  (let ((def (lookup term))
        (simple-def-result (make-simple-definitions-result)))
    (let* ((object ((definition-generator def)))
           (fig (figure (as-premise (name-polygon object) 0)))
           (all-observations (analyze-figure fig))
           (eligible-observations
            (filter observation->constraint all-observations)))
      (for-each
       (lambda (obs-subset)
         (if (simple-def-should-test? simple-def-result obs-subset)
             (let ((polygon
                    (polygon-from-object-observations object obs-subset)))
               ((cond ((false? polygon) mark-unknown-simple-def!)
                      ((is-a? term polygon)
                       (begin (pp "=> Sufficient")
                              mark-sufficient-simple-def!))
                      (else (begin (pp "=> Insufficient")
                                   mark-insufficient-simple-def!)))
                simple-def-result obs-subset)
               (simplify-definitions-result! simple-def-result))
             (pprint `(skipping ,obs-subset))))
       (shuffle (all-subsets eligible-observations)))
      (pprint simple-def-result)
      simple-def-result)))


(define (get-simple-definitions term)
  (let ((def (lookup term))
        (simple-def-result (make-simple-definitions-result)))
    (let* ((object ((definition-generator def)))
           (fig (figure (as-premise (name-polygon object) 0)))
           (all-observations (analyze-figure fig))
           (eligible-observations
            (filter observation->constraint all-observations)))
      (for-each
       (lambda (obs-subset)
         (if (simple-def-should-test? simple-def-result obs-subset)
             (let ((polygon
                    (polygon-from-object-observations object obs-subset)))
               ((cond ((false? polygon) mark-unknown-simple-def!)
                      ((is-a? term polygon) mark-sufficient-simple-def!)
                      (else mark-insufficient-simple-def!))
                simple-def-result obs-subset)
               (simplify-definitions-result! simple-def-result))
             (pprint `(skipping ,obs-subset))))
       (shuffle (all-subsets eligible-observations)))
      (pprint simple-def-result)
      simple-def-result)))
