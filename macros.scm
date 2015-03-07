;;; Syntax for setting names for geometry objects declared via let-geo
(define-syntax let-geo*
  (sc-macro-transformer
   (lambda (exp env)
     (let ((assignments (cadr exp))
           (body (caddr exp)))
       (let ((new-body
              (map (lambda (a)
                     (let  ((symb (car a)))
                       `(set-element-name! ,symb (quote ,symb))))
                   assignments)))
         `(let*
              ,assignments
            ,@new-body
            ,body
            ))))))
