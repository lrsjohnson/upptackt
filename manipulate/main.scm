;;; main.scm --- Main definitions and code for running the
;;; manipulation / mechanism-based code

;;; Examples

(define (arbitrary-triangle)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c)))

(define (run-mechanism mechanism-proc)
  (initialize-scheduler)
  (let ((m (mechanism-proc)))
    (pp (m:joint-anchored? (car (m:mechanism-joints m))))
    (m:build-mechanism m)
    (let ((f (m:mechanism->figure m)))
      (draw-figure f c)
      (pp (analyze-figure f)))))

#|
(let ((m (m:mechanism
          (m:establish-polygon-topology 'a 'b 'c 'd)
          (m:c-angle-equal (m:joint 'a)
                           (m:joint 'c))
          (m:c-length-equal (m:bar 'b 'c)
                            (m:bar 'a 'd)) )))
  (m:build-mechanism m)
  (m:draw-mechanism m c))
|#
