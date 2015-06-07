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

(define (quadrilateral-with-diagonals a b c d)
  (list
   (m:establish-polygon-topology a b c d)
   (m:establish-polygon-topology a b c)
   (m:establish-polygon-topology b c d)
   (m:establish-polygon-topology c d a)
   (m:establish-polygon-topology d a c)))

(define (quadrilateral-with-diagonals-intersection a b c d e)
  (list
   (quadrilateral-with-diagonals a b c d)
   (m:establish-polygon-topology a b e)
   (m:establish-polygon-topology b c e)
   (m:establish-polygon-topology c d e)
   (m:establish-polygon-topology d a e)
   (m:c-line-order c e a)
   (m:c-line-order b e d)))

(define (quad-diagonals)
  (m:mechanism
   ;; Setup abcd with e in the middle:
   (quadrilateral-with-diagonals-intersection 'a 'b 'c 'd 'e)

   ;; Right Angle in Center:
   (m:c-right-angle (m:joint 'b 'e 'c))

   ;; Diagonals Equal
   ;;(m:c-length-equal (m:bar 'c 'a) (m:bar 'b 'd))
   (m:c-length-equal (m:bar 'c 'e) (m:bar 'a 'e))
   ;;(m:c-length-equal (m:bar 'b 'e) (m:bar 'd 'e))

   ;; Make it a square:
   ;;(m:c-length-equal (m:bar 'c 'e) (m:bar 'b 'e))
   ))

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
                    (m:joint 'b))
   (m:equal-joints-in-sum
    (list (m:joint 'a) (m:joint 'b))
    (list (m:joint 'a) (m:joint 'b) (m:joint 'c))
    pi)))

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

(define (parallelogram-by-angles)
  (m:mechanism
   (m:establish-polygon-topology 'a 'b 'c 'd)
   (m:c-angle-equal (m:joint 'a)
                    (m:joint 'c))
   (m:c-angle-equal (m:joint 'b)
                    (m:joint 'd))))

(define *m*)
(define (m:run-mechanism mechanism-proc)
  (initialize-scheduler)
  (let ((m (mechanism-proc)))
    (set! *m* m)
    (m:build-mechanism m)
    (m:solve-mechanism m)
    (let ((f (m:mechanism->figure m)))
      (draw-figure f c)
      ;;(pp (analyze-figure f))
      )))

#|
 (let lp ()
   (initialize-scheduler)
   (pp 'start)
   (m:run-mechanism
    (lambda ()
      (m:mechanism
       ;;(m:establish-polygon-topology 'a 'b 'c)
       (m:make-named-bar 'a 'b)
       (m:make-named-bar 'b 'c)
       (m:make-named-bar 'c 'a)
       (m:make-named-joint 'c 'b 'a)
       (m:make-named-joint 'a 'c 'b)
       (m:make-named-joint 'b 'a 'c)

       (m:make-named-bar 'a 'd)
       (m:make-named-bar 'b 'd)
       (m:make-named-joint 'd 'a 'b)
       (m:make-named-joint 'a 'b 'd)
       (m:make-named-joint 'b 'd 'a)

       (m:make-named-bar 'c 'd)
       (m:make-named-joint 'a 'd 'c)
       (m:make-named-joint 'c 'a 'd)
       (m:make-named-joint 'd 'c 'a))))
   (lp))

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

(define (rect-demo-1)
  (m:mechanism
   (m:establish-polygon-topology 'a 'b 'c 'd)
   (m:c-length-equal (m:bar 'a 'b)
                     (m:bar 'b 'c))
   (m:c-right-angle (m:joint 'd))))

(define (rect-demo-2)
  (m:mechanism
   (m:establish-polygon-topology 'a 'b 'c 'd)
   (m:c-length-equal (m:bar 'a 'd)
                     (m:bar 'b 'c))
   (m:c-right-angle (m:joint 'd))
   (m:c-angle-equal (m:joint 'a)
                    (m:joint 'c))))

(define (rect-demo-3)
  (m:mechanism
   (m:establish-polygon-topology 'a 'b 'c 'd)
   (m:c-length-equal (m:bar 'a 'd)
                     (m:bar 'b 'c))
   (m:c-right-angle (m:joint 'd))
   (m:c-right-angle (m:joint 'b))))
