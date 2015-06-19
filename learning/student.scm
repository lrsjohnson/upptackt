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
  (make-lattice less-specific?
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
   (definition-lattice) (make-lattice-node term term)))

(define (add-definition! def)
  (let ((term (definition-name def)))
    (if (lookup-definition name)
        (error "Definition already exists for" term))
    (save-definition! def)
    (add-definition-lattice-node! term)
    (update-definitions-after-added term)))

;;;;;;;;;;;;;;;;;;;;;;;;; Student Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (term-known? term)
  (lookup-definition term))

(define (is-a? term obj)
  (let ((def (lookup term)))
    (definition-holds? def obj)))

;;;;;;;;;;;;;;;;;;;;;;;; Initializing Student ;;;;;;;;;;;;;;;;;;;;;;;;

(define (learn-term term object-generator)
  (if (term-known? term)
      (error "Term already known:" term))
  (let ((example (name-polygon (object-generator))))
    (let* ((classifications (examine example))
           (fig (figure
                 (with-source (lambda (p) (car p))
                  (with-dependency '<premise> example))))
           (observations (analyze-figure fig))
           (conjectures (map conjecture-from-observation observations)))
      (pprint conjectures)
      (let ((new-def
             (make-restrictions-definition
              term
              classifications
              conjectures
              object-generator)))
        (add-definition! new-def)
        'done))))

(define (update-definition-classificiations term)
  (let* ((def (lookup term))
         (current-conjectures (definition-conjectures def))
         (ancestor-terms (ancestor-terms term))
         (ancestor-defs (map lookup ancestor-terms))
         (ancestor-conjectures
          (append-map definition-conjectures ancestor-defs))
         (new-conjectures
          (set-difference current-conjectures
                          ancestor-conjectures
                          conjecture-equal?)))
    (set-definition-conjectures!
     def
     new-conjectures)))

(define (lattice-node-for-term term)
  (lattice-node-by-key (definition-lattice) term))

(define (child-terms term)
  (let* ((lattice-node (lattice-node-for-term term))
         (child-nodes (lattice-node-children lattice-node)))
    (map lattice-node-key child-nodes)))

(define (ancestor-terms term)
  (let ((ancestor-nodes (sublattice-nodes-upwards
                         (definition-lattice)
                         term)))
    (delq term (map lattice-node-key ancestor-nodes))))


(define (update-definitions-after-added new-term)
  (let* ((child-terms (child-terms new-term)))
    (update-definition-classificiations new-term)
    (for-each update-definition-classificiations
              child-terms)))

;;;;;;;;;;;;;;;;;;;;;; Simplifying Definitions ;;;;;;;;;;;;;;;;;;;;;;;

(define (get-simple-definitions term)
  (let ((def (lookup term)))
    (if (unknown? def)
        (error "Unknown term" term))
    (let* ((object ((definition-generator def)))
           (observations
            (filter
             observation->constraint
             (all-observations
              (figure (name-polygon object))))))
      (map
       (lambda (obs-subset)
         (pprint obs-subset)
         (let* ((topology (topology-for-object object))
                (new-figure
                 (observations->figure topology obs-subset)))
           (if new-figure
               (let ((new-polygon
                      (polygon-from-figure new-figure)))
                 (pprint new-polygon)
                 (if (is-a? term new-polygon)
                     (list 'valid-definition
                           obs-subset)
                     (list 'invalid-definition
                           obs-subset)))
               (list 'unknown-definition
                     obs-subset))))
       (all-subsets observations)))))
