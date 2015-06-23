;;; lattice.scm -- code for general lattice

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Nodes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <lattice-node>
  (%make-lattice-node key content parents children)
  lattice-node?
  (key lattice-node-key)
  (content lattice-node-content)
  (parents lattice-node-parents set-lattice-node-parents!)
  (children lattice-node-children set-lattice-node-children!))

(define (make-lattice-node key content)
  (%make-lattice-node key content '() '()))

(define (add-lattice-node-parent! node parent-node)
  (set-lattice-node-parents!
   node
   (cons parent-node (lattice-node-parents node))))

(define (add-lattice-node-child! node child-node)
  (set-lattice-node-children!
   node
   (cons child-node (lattice-node-children node))))

(define (add-lattice-node-children! node children-nodes)
  (for-each
   (lambda (child)
           (add-lattice-node-child! node child))
   children-nodes))

(define (print-lattice-node node)
  (list (lattice-node-key node)
        (lattice-node-content node)
        (map lattice-node-key (lattice-node-parents node))
        (map lattice-node-key (lattice-node-children node))))

(defhandler print print-lattice-node lattice-node?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lattice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Partial-order-proc is a procedure on keys that returns true if the
;;; first argument is a parent of "above" the second in the lattice

(define-record-type <lattice>
  (%make-lattice partial-order-proc root node-index)
  lattice?
  (partial-order-proc lattice-partial-order-proc)
  (root lattice-root)
  (node-index lattice-node-index))

(define (make-lattice partial-order-proc root)
  (define (node-partial-order-proc parent-node child-node)
    (partial-order-proc
     (lattice-node-content parent-node)
     (lattice-node-content child-node)))
  (let ((node-index (make-key-weak-eq-hash-table)))
    (hash-table/put! node-index
                     (lattice-node-key root)
                     root)
    (%make-lattice node-partial-order-proc root
                   node-index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Index by Key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lattice-node-by-key lattice key)
  (hash-table/get
   (lattice-node-index lattice)
   key
   #f))

(define (lattice-keys lattice)
  (hash-table/key-list
   (lattice-node-index lattice)))

(define (lattice-nodes lattice)
  (hash-table/datum-list
   (lattice-node-index lattice)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Querying Lattice ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sublattice downwards from node
(define (sublattice-nodes lattice start-key)
  (sublattice-nodes-from-key-with-getter
   lattice start-key lattice-node-children))

(define (sublattice-nodes-upwards lattice start-key)
  (sublattice-nodes-from-key-with-getter
   lattice start-key lattice-node-parents))

(define (sublattice-nodes-from-key-with-getter
         lattice start-key next-nodes-getter)
  (let ((visited '())
        (start-node (lattice-node-by-key lattice start-key)))
    (define (visited? node)
      (memq (lattice-node-key node) visited))
    (define (mark-visited node)
      (set! visited (cons (lattice-node-key node) visited)))
    (define (get-unvisited nodes)
      (let ((unvisited-nodes
             (filter (notp visited?)
                     nodes)))
        (for-each mark-visited unvisited-nodes)
        unvisited-nodes))
    (mark-visited start-node)
    (let lp ((agenda (list start-node))
             (sublattice-nodes (list start-node)))
      (if (null? agenda)
          sublattice-nodes
          (let ((node (car agenda)))
            (let ((unvisited-nodes
                   (get-unvisited (next-nodes-getter node))))
              (lp (append (cdr agenda) unvisited-nodes)
                  (append sublattice-nodes unvisited-nodes))))))))

;;;;;;;;;;;;;;;;;;;;;;;;; Adding to Lattice ;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (add-lattice-node lattice new-node)
  (if (lattice-node-by-key lattice (lattice-node-key new-node))
      'done
      (let ((visited '()))
        (hash-table/put!
         (lattice-node-index lattice)
         (lattice-node-key new-node)
         new-node)
        (define (visited? node)
          (memq (lattice-node-key node) visited))
        (define (mark-visited node)
          (set! visited (cons (lattice-node-key node) visited)))
        (define (ancestor-of-new-node? node)
          ((lattice-partial-order-proc lattice) node new-node))
        (define (descendent-of-new-node? node)
          ((lattice-partial-order-proc lattice) new-node node))
        (define (get-unvisited nodes)
          (let ((unvisited-nodes
                 (filter (notp visited?) nodes)))
            (for-each mark-visited unvisited-nodes)
            unvisited-nodes))
        (define (save-as-parent parent-node)
          (add-lattice-node-parent! new-node parent-node)
          (let lp ((agenda (list parent-node)))
            (if (null? agenda) 'done
                (let ((node (car agenda)))
                  (let ((children (lattice-node-children node)))
                    (let ((descendent-children
                           (filter descendent-of-new-node?
                                   children))
                          (nondescendent-children
                           (filter (notp descendent-of-new-node?)
                                   children)))
                      (add-lattice-node-children!
                       new-node descendent-children)
                      (lp (append (cdr agenda)
                                  (get-unvisited
                                   nondescendent-children)))))))))
        (let lp ((agenda (list (lattice-root lattice))))
          (if (null? agenda)
              (update-parent-child-pointers lattice new-node)
              (let ((node (car agenda)))
                (let ((children (lattice-node-children node)))
                  (let ((ancestor-children
                         (filter ancestor-of-new-node?
                                 children)))
                    (if (null? ancestor-children)
                        (begin (save-as-parent node)
                               (lp (cdr agenda)))
                        (lp (append (cdr agenda)
                                    (get-unvisited
                                     ancestor-children))))))))))))

(define (clean-children lattice node)
  (let ((children (dedupe-by eq? (lattice-node-children node))))
    (set-lattice-node-children!
     node
     (remove-supplanted
      (lattice-partial-order-proc lattice)
      children))))

(define (clean-parents lattice node)
  (let ((parents (dedupe-by eq? (lattice-node-parents node))))
    (set-lattice-node-parents!
     node
     (remove-supplanted
      (flip-args (lattice-partial-order-proc lattice))
      parents))))

(define (update-parent-child-pointers lattice new-node)
  (let ((parents-of-new-node (lattice-node-parents new-node))
        (children-of-new-node (lattice-node-children new-node)))
    (for-each (lambda (parent-node)
                (set-lattice-node-children!
                 parent-node
                 (set-difference
                  (cons new-node (lattice-node-children parent-node))
                  children-of-new-node
                  eq?))
                (clean-children lattice parent-node))
              parents-of-new-node)
    (for-each (lambda (child-node)
                (set-lattice-node-parents!
                 child-node
                 (set-difference
                  (cons new-node (lattice-node-parents child-node))
                  parents-of-new-node
                  eq?))
                (clean-parents lattice child-node))
              children-of-new-node)
    (clean-children lattice new-node)
    (clean-parents lattice new-node)))

(define (remove-lattice-node lattice node-key)
  (let* ((node-to-remove (lattice-node-by-key lattice node-key))
         (children-of-removed-node
          (lattice-node-children node-to-remove))
         (parents-of-removed-node
          (lattice-node-parents node-to-remove)))
    (hash-table/remove! (lattice-node-index lattice)
                        node-key)
    (for-each (lambda (parent-node)
                (set-lattice-node-children!
                 parent-node
                 (append
                  (delq node-to-remove
                        (lattice-node-children parent-node))
                  children-of-removed-node))
                (clean-children lattice parent-node))
              parents-of-removed-node)
    (for-each (lambda (child-node)
                (set-lattice-node-parents!
                 child-node
                 (append
                  (delq node-to-remove
                        (lattice-node-parents child-node))
                  parents-of-removed-node))
                (clean-parents lattice child-node))
              children-of-removed-node)))

;;;;;;;;;;;;;;;;;;;;;;;;; Dot Visualization ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Replace - with _
(define (dot-encode-symbol symbol)
  (list->string
   (map (lambda (char)
          (if (char=? char #\-)
              #\_
              char))
        (string->list (symbol->string symbol)))))

(define (lattice-node->string node)
  (let ((key (lattice-node-key node))
        (content (lattice-node-content node)))
    (string-append
     (symbol->string key)
     (if (not (eq? key content))
         (with-output-to-string
           (lambda ()
             (write-string "\n")
             (write (print content))))
         ""))))

(define (lattice-nodes->dot-string lattice-nodes)
  (string-append
   "digraph G {"
   (apply
    string-append
    (append-map
     (lambda (node)
       (let ((node-key (lattice-node-key node)))
        (cons
         (string-append
          (dot-encode-symbol node-key)
          "[label=\"" (lattice-node->string node) "\"];\n")
         (map (lambda (child-node)
                (string-append
                 (dot-encode-symbol node-key)
                 " -> "
                 (dot-encode-symbol (lattice-node-key child-node))
                 ";\n"))
              (lattice-node-children node)))))
     lattice-nodes))
   "}\n"))

(define (show-lattice-nodes lattice-nodes)
  (let ((dot-string (lattice-nodes->dot-string lattice-nodes)))
    (call-with-output-file "/tmp/lattice.dot"
      (lambda (dot-file)
        (write-string dot-string dot-file)))
    (run-shell-command "rm /tmp/lattice.png")
    (run-shell-command "dot -Tpng -o /tmp/lattice.png /tmp/lattice.dot")
    (run-shell-command "open /tmp/lattice.png")))

(define (show-lattice lattice)
  (show-lattice-nodes (lattice-nodes lattice)))

(define (show-lattice-from-key lattice key)
  (show-lattice-nodes
   (sublattice-nodes lattice key)))

;;; Example:

#|
(let* ((root (make-lattice-node 'root '()))
       (lattice (make-lattice eq-subset? root))
       (a (make-lattice-node 'a '(1)))
       (b (make-lattice-node 'b '(2)))
       (c (make-lattice-node 'c '(3)))
       (d (make-lattice-node 'd '(1 2)))
       (e (make-lattice-node 'e '(1 3)))
       (f (make-lattice-node 'f '(2 3 4)))
       (g (make-lattice-node 'g '(1 2 3)))
       (h (make-lattice-node 'h '(1 2 3 4))))
  (add-lattice-node lattice root)
  (add-lattice-node lattice c)
  (add-lattice-node lattice h)
  (add-lattice-node lattice f)
  (add-lattice-node lattice e)
  (add-lattice-node lattice g)
  (add-lattice-node lattice a)
  (add-lattice-node lattice d)
  (add-lattice-node lattice b)
  (pprint root)
  (pprint a)
  (pprint b)
  (pprint c)
  (pprint d)
  (pprint e)
  (pprint f)
  (pprint g)
  (pprint h)
  (remove-lattice-node lattice 'd)
  (show-lattice-from-key lattice 'root))

; ->
(root () () (a c b))
(a (1) (root) (e d))
(b (2) (root) (d f))
(c (3) (root) (f e))
(d (1 2) (a b) (g))
(e (1 3) (c a) (g))
(f (2 3 4) (c b) (h))
(g (1 2 3) (d e) (h))
(h (1 2 3 4) (g f) ())
|#
