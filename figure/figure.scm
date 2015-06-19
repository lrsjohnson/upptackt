;;; figure.scm --- Figure

;;; Commentary:

;; Ideas:
;; - Gathers elements that are part of a figure
;; - Helpers to extract relevant elements

;; Future:
;; - Convert to record type like other structures
;; - Extract points automatically?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; Figure Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (figure . elements)
  (cons 'figure elements))
(define (figure-elements figure)
  (cdr figure))

(define (all-figure-elements figure)
  (append (figure-elements figure)
          (figure-points figure)
          (figure-linear-elements figure)))

(define (figure? x)
  (and (pair? x)
       (eq? (car x 'figure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Getters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (figure-filter predicate figure)
  (filter predicate (figure-elements figure)))

(define (figure-points figure)
  (dedupe-by point-equal?
             (append (figure-filter point? figure)
                     (append-map (lambda (polygon) (polygon-points polygon))
                                 (figure-filter polygon? figure))
                     (append-map (lambda (s)
                                   (list (segment-endpoint-1 s)
                                         (segment-endpoint-2 s)))
                                 (figure-filter segment? figure))
                     (map (lambda (a)
                            (angle-vertex a))
                          (figure-filter angle? figure)))))

(define (figure-angles figure)
  (append (figure-filter angle? figure)
          (append-map (lambda (polygon) (polygon-angles polygon))
                      (figure-filter polygon? figure))))

(define (figure-polygons figure)
  (figure-filter polygon? figure))

(define (figure-segments figure)
  (append (figure-filter segment? figure)
          (append-map (lambda (polygon) (polygon-segments polygon))
                      (figure-filter polygon? figure))))

(define (figure-linear-elements figure)
  (append (figure-filter linear-element? figure)
          (append-map (lambda (polygon) (polygon-segments polygon))
                      (figure-filter polygon? figure))))
