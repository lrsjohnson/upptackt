1 ]=> (enumerate-graphics-types)

;Loading "/Applications/mit-scheme.app/Contents/Resources/lib/prx11.so"... done
;Value 13: (#[graphics-type 14 x])

1 ]=> (enumerate-graphics-types)

;Value 15: (#[graphics-type 14 x])

1 ]=> (car (enumerate-graphics-types))

;Value 14: #[graphics-type 14 x]

1 ]=> (make-graphics-device (car (enumerate-graphics-types)))

;Value 16: #[graphics-device 16]

1 ]=> (define scheme-g (make-graphics-device (car (enumerate-graphics-types))))

;Value: scheme-g

1 ]=> (define g scheme-g)

;Value: g

1 ]=> (graphics-clear g)

;Unspecified return value

1 ]=> (graphics-coordinate-limits g)

;Value 17: #[compiled-closure 17 ("x11graph" #x80) #x380 #x10d9d3900 #x10ed477b8]

1 ]=> (graphics-device-coordinate-limits g)

;Value 18: #[compiled-closure 18 ("x11graph" #x81) #x283 #x10d9d3bfb #x10ed4f350]

1 ]=> ((graphics-coordinate-limits g) 'x-left)

;The object x-left is not applicable.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify a procedure to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

2 error> (pp (graphics-coordinate-limits g))
(lambda (receiver)
  (receiver value-0 value-1 value-2 value-3))
;Unspecified return value

2 error> ((graphics-coordinate-limits g) (lambda (xl yb xr yt) (pp (list xl yb xr yt))
)
  C-c C-c
;Quit!

1 ]=> ((graphics-coordinate-limits g) (lambda (xl yb xr yt) (pp (list xl yb xr yt))))
(-1. -1. 1. 1.)
;Unspecified return value

1 ]=> ((graphics-coordinate-limits g) (lambda (xl yb xr yt) (pp (list xl yb xr yt))))
(-1. -1. 1. 1.)
;Unspecified return value

1 ]=> (graphics-draw-point g 0.5 0.5)

;Unspecified return value

1 ]=> (graphics-draw-line g -0.5 -0.5 0.5 0.5)

;Unspecified return value

1 ]=> (graphics-draw-line g -0.5 -0.5 0.5 0.5)
