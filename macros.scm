;;; macros.scm --- Macros for let-geo* to assign names and variables
;;; to elements

;;; Commentary:

;; Ideas:
;; - Basic naming
;; - Multiple assignment

;; Future:
;; - Warn about more errors
;; - More efficient multiple-assignment for lists

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Expanding Assignment ;;;;;;;;;;;;;;;;;;;;;;;;

(define *multiple-assignment-symbol* '*multiple-assignment-result*)

(define (expand-multiple-assignment lhs rhs)
  (expand-compound-assignment
   (list *multiple-assignment-symbol* lhs)
   rhs))

(define (make-component-assignments key-name component-names)
  (map (lambda (name i)
         (list name `(element-component ,key-name ,i)))
       component-names
       (iota (length component-names))))

(define (expand-compound-assignment lhs rhs)
  (if (not (= 2 (length lhs)))
      (error "Malformed compound assignment LHS (needs 2 elements): " lhs))
  (let ((key-name (car lhs))
        (component-names (cadr lhs)))
    (if (not (list? component-names))
        (error "Component names must be a list:" component-names))
    (let ((main-assignment (list key-name rhs))
          (component-assignments (make-component-assignments
                                  key-name
                                  component-names)))
      (cons main-assignment
            component-assignments))))

(define (expand-assignment assignment)
  (if (not (= 2 (length assignment)))
      (error "Assignment in letgeo* must be of length 2, found:" assignment))
  (let ((lhs (car assignment))
        (rhs (cadr assignment)))
    (if (list? lhs)
        (if (= (length lhs) 1)
            (expand-multiple-assignment (car lhs) rhs)
            (expand-compound-assignment lhs rhs))
        (list assignment))))

(define (expand-assignments assignments)
  (append-map expand-assignment assignments))

;;;;;;;;;;;;;;;;;;;;;;; Extract Variable Names ;;;;;;;;;;;;;;;;;;;;;;;

(define (variables-from-assignment assignment)
  (flatten (list (car assignment))))

(define (variables-from-assignments assignments)
  (append-map variables-from-assignment assignments))

(define (set-name-expressions symbols)
  (map (lambda (s)
         `(set-element-name! ,s (quote ,s)))
       symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let-geo* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Syntax for setting names for geometry objects declared via let-geo
(define-syntax let-geo*
  (sc-macro-transformer
   (lambda (exp env)
     (let ((assignments (cadr exp))
           (body (caddr exp)))
       (let ((new-assignments (expand-assignments assignments))
             (variable-names (variables-from-assignments assignments)))
         (let ((result`(let*
                           ,new-assignments
                         ,@(set-name-expressions variable-names)
                         ,body)))
           result))))))
