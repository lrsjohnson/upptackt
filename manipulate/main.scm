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
    (m:solve-mechanism m)
    (let ((f (m:mechanism->figure m)))
      (draw-figure f c)
      (pp (analyze-figure f)))))
#|
(let lp ()
  (initialize-scheduler)
  (let ((m (m:mechanism
            (m:establish-polygon-topology 'a 'b 'c 'd))))
    (m:build-mechanism m)
    (m:solve-mechanism m)
    (let ((f (m:mechanism->figure m)))
      (draw-figure f c)
      (pp (analyze-figure f)))))
|#
