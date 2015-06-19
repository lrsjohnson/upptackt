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
  (hash-table/get (student-definitions s)
                  name
                  #f))

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
    (let* ((primitives (examine-primitive example))
           (fig (figure (as-premise example)))
           (observations (analyze-figure fig))
           (conjectures (map conjecture-from-observation observations)))
      (pprint conjectures)
      (let ((new-def
             (make-restrictions-definition
              term
              object-generator
              (definition-predicate (lookup (car primitives)))
              conjectures)))
        (add-definition! new-def)
        (check-new-def new-def)
        'done))))

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
          (append-map definition-conjectures ancestor-defs)))
    (append (definition-conjectures (lookup term))
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

;;;;;;;;;;;;;;;;;;;;; Performing Investigations ;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;; Simplifying Definitions ;;;;;;;;;;;;;;;;;;;;;;;

(define (polygon-from-object-observations object obs-subset)
  (let* ((topology (topology-for-object object))
         (new-figure (observations->figure topology obs-subset)))
    (and new-figure
         (object-from-new-figure object new-figure))))

(define (get-simple-definitions term)
  (let ((def (lookup term))
        (simple-def-result (make-simple-definitions-result)))
    (let* ((object ((definition-generator def)))
           (fig (figure (as-premise (name-polygon object))))
           (all-observations (analyze-figure fig))
           (eligible-observations
            (filter observation->constraint
                    all-observations)))
      (for-each
       (lambda (obs-subset)
         (let ((polygon (polygon-from-object-observations object obs-subset)))
           ((cond ((false? polygon) mark-unknown-simple-def!)
                  ((is-a? term polygon) mark-valid-simple-def!)
                  (else mark-invalid-simple-def!))
            simple-def-result obs-subset)))
       (all-subsets eligible-observations))
      (simplify-definitions-result! simple-def-result)
      simple-def-result)))
