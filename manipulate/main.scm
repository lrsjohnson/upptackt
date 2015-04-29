;;; main.scm --- Main definitions and code for running the
;;; manipulation / mechanism-based code

;;; Examples

(define (arbitrary-triangle)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c)))

(define (arbitrary-right-triangle)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c)
  (m:c-right-angle (m:joint 'a))))

(define (arbitrary-right-triangle-2)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c)
  (m:c-right-angle (m:joint 'c))))

;;; Works:
(define (isoceles-triangle)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c)
  (m:c-length-equal (m:bar 'a 'b)
                    (m:bar 'b 'c))))

(define (isoceles-triangle-by-angles)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c)
  (m:c-angle-equal (m:joint 'a)
                   (m:joint 'b))))

;;; Often works:
(define (arbitrary-quadrilateral)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c 'd)))

;;; Always works:
(define (parallelogram-by-sides)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c 'd)
  (m:c-length-equal (m:bar 'a 'b)
                    (m:bar 'c 'd))
  (m:c-length-equal (m:bar 'b 'c)
                    (m:bar 'd 'a))))

(define (kite-by-sides)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c 'd)
  (m:c-length-equal (m:bar 'a 'b)
                    (m:bar 'b 'c))
  (m:c-length-equal (m:bar 'c 'd)
                    (m:bar 'd 'a))))

(define (rhombus-by-sides)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c 'd)
  (m:c-length-equal (m:bar 'a 'b)
                    (m:bar 'b 'c))
  (m:c-length-equal (m:bar 'b 'c)
                    (m:bar 'c 'd))
  (m:c-length-equal (m:bar 'c 'd)
                    (m:bar 'a 'd))))

;;; Never works:
(define (parallelogram-by-angles)
 (m:mechanism
  (m:establish-polygon-topology 'a 'b 'c 'd)
  (m:c-angle-equal (m:joint 'a)
                   (m:joint 'c))
  (m:c-angle-equal (m:joint 'b)
                   (m:joint 'd))))


(define (m:run-mechanism mechanism-proc)
  (initialize-scheduler)
  (let ((m (mechanism-proc)))
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
