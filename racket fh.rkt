#lang racket
(require graph)

(define g (weighted-graph/directed '((x a b) (y b c))))

(define pdl "x;y")

(define (verificaPDLGrafo programaPDL i grafo listaInterna)
  
    (cond 
     
      [(char=? (string-ref programaPDL i) #\x) ;Se o caractere na posição atual é igual a "x"
        (display "aaaaaaaaaaaaaaaaaaaaaa")
        (append listaInterna (list(string-ref programaPDL i)) ;Coloca no final da listaInterna o valor achado
        (verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna))
      ]

      [(char=? (string-ref programaPDL i) #\y) ;Se o caractere na posição atual é igual a "y"
        (append listaInterna (list(string-ref programaPDL i)) ;Coloca no final da listaInterna o valor achado
        (verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna))
      ]

      [(char=? (string-ref programaPDL i) #\U) ;Se o caractere na posição atual é igual a "U"
        (append listaInterna (list(string-ref programaPDL i)) ;Coloca no final da listaInterna o valor achado
        (verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna))
      ]

      [(char=? (string-ref programaPDL i) #\*) ;Se o caractere na posição atual é igual a "*"
        (append listaInterna (list(string-ref programaPDL i)) ;Coloca no final da listaInterna o valor achado
        (verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna))
      ]

      [(char=? (string-ref programaPDL i) #\;) ;Se o caractere na posição atual é igual a ";"
        (display listaInterna)      ]
     
      ;[(not (char=? (string-ref programaPDL i) #\;)) ;se não, percorre a palavra até achar
      ;  (if (not (= (- (string-length programaPDL) 1) i)) ; condição de parada: i = string-length
      ;    (verificaPDLGrafo programaPDL (+ i 1)) ; imprime caracteres recursivamente
      ;    (display "" ))]

    )
  
)