#lang racket
(require graph)

(define g (weighted-graph/directed '((a x y) (b y z))))

(define pdl "a;b")

(define (printaCaractere palavra i)
  
   (display (string-ref palavra i))
   
   (if (not (= (- (string-length palavra) 1) i))
       (printaCaractere palavra (+ i 1))
       (display "" )
   )
)