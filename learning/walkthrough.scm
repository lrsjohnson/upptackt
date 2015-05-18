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

(is-a? 'polygon (random-square))

(is-a? 'quadrilateral (random-square))

(is-a? 'triangle (random-square))

(is-a? 'segment (random-square))

(is-a? 'line (random-line))

;;;;;;;;;;;;;;;;;; Can learn and explain new terms ;;;;;;;;;;;;;;;;;;;

(what-is 'isoc-t)

(learn-term 'isoc-t random-isoceles-triangle)

(what-is 'isoc-t)

(is-a? 'isoc-t (random-isoceles-triangle))

(is-a? 'isoc-t (random-equilateral-triangle))

(is-a? 'isoc-t (random-triangle))

(learn-term 'equi-t random-equilateral-triangle)

(what-is 'equi-t)

(is-a? 'equi-t (random-isoceles-triangle))

(is-a? 'equi-t (random-equilateral-triangle))

;;;;;;;;;;;;;;; Let's learn some basic quadrilaterals ;;;;;;;;;;;;;;;;

(learn-term 'pl random-parallelogram)

(what-is 'pl)

(learn-term 'kite random-kite)

(what-is 'kite)

(learn-term 'rh random-rhombus)

(what-is 'rh)

(learn-term 'rectangle random-rectangle)

(what-is 'rect)

(learn-term 'sq random-square)

(what-is 'sq)
