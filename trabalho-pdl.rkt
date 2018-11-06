#lang racket
(require graph)

(define g (weighted-graph/directed '((a x y) (b y z))))

(define pdl "a;b")

(define (printaCaractere palavra i)

  (display (string-ref palavra i)) ; mostrar o caractere na posição i
   (cond ; cond é tipo um switch
     
     [(char=? (string-ref palavra (+ 1 i)) #\;) ;se próximo caracter é ;
      (display "" )]
     
     [(not (char=? (string-ref palavra i) #\;)) ;se não, percorre a palavra até achar
      (if (not (= (- (string-length palavra) 1) i)) ; condição de parada: i = string-length
       (printaCaractere palavra (+ i 1)) ; imprime caracteres recursivamente
       (display "" ))]

   )
  
)

