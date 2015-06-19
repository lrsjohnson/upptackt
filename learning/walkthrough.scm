;;; Sample:

;;;;;;;;;;;;;;;;;;;;;;;;;; Looking up terms ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Starts with limited knowledge

(what-is 'square)

(what-is 'rhombus)

;;; Knows primitive objects

(what-is 'line)

(what-is 'point)

(what-is 'polygon)

;;; And some built-in non-primitives

(what-is 'triangle)

(what-is 'quadrilateral)

;;;;;;;;;;;; Can idenitfy whether elements satisfy these ;;;;;;;;;;;;;

(show-element (random-parallelogram))

(is-a? 'polygon (random-square))

(is-a? 'quadrilateral (random-square))

(is-a? 'triangle (random-square))

(is-a? 'segment (random-square))

(is-a? 'line (random-line))

;;;;;;;;;;;;;;;;;; Can learn and explain new terms ;;;;;;;;;;;;;;;;;;;

(what-is 'isoceles-triangle)

(learn-term 'isoceles-triangle random-isoceles-triangle)

(what-is 'isoceles-triangle)

(is-a? 'isoceles-triangle (random-isoceles-triangle))

(is-a? 'isoceles-triangle (random-equilateral-triangle))

(is-a? 'isoceles-triangle (random-triangle))

(learn-term 'equilateral-triangle random-equilateral-triangle)

(what-is 'equilateral-triangle)

(is-a? 'equilateral-triangle (random-isoceles-triangle))

(is-a? 'equilateral-triangle (random-equilateral-triangle))

;;;;;;;;;;;;;;; Let's learn some basic quadrilaterals ;;;;;;;;;;;;;;;;

(learn-term 'kite random-kite)

(what-is 'kite)

(learn-term 'rectangle random-rectangle)

(what-is 'rectangle)

(learn-term 'trapezoid random-trapezoid)

(what-is 'trapezoid)

(learn-term 'square random-square)

(what-is 'square)

(learn-term 'orthodiagonal random-orthodiagonal-quadrilateral)

(what-is 'orthodiagonal)

(learn-term 'parallelogram random-parallelogram)

(what-is 'parallelogram)

(learn-term 'rhombus random-rhombus)

(what-is 'rhombus)

(learn-term 'equidiagonal random-equidiagonal-quadrilateral)

(what-is 'equidiagonal)

(learn-term 'cyclic random-cyclic-quadrilateral)

(what-is 'cyclic)

(learn-term 'isoceles-trapezoid random-isoceles-trapezoid)

(what-is 'isoceles-trapezoid)

(learn-term 'three-equal-trapezoid random-3-equal-trapezoid)

(what-is 'three-equal-trapezoid)


;;; (run-investigation (diagonal-investigation))
;;; (run-investigation (midsegment-investigation))

;;; (show-definition-lattice)
