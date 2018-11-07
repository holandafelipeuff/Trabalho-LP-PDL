#lang racket

(require graph)

(define g (weighted-graph/directed '((a x y) (b y z))))

(define pdl "a;b")

(define (verificaPDLGrafo programaPDL i grafo listaInterna estadoAtual)
  
    (cond 
     
      [(char=? (string-ref programaPDL i) #\a) ;Se o caractere na posição atual é igual a "a"
        (set! listaInterna (append listaInterna (list(string-ref programaPDL i)))) ;Coloca no final da listaInterna o valor achado
        (verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
      ]

      [(char=? (string-ref programaPDL i) #\b) ;Se o caractere na posição atual é igual a "b"
        (set! listaInterna (append listaInterna (list(string-ref programaPDL i)))) ;Coloca no final da listaInterna o valor achado
        (verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
      ]

      [(char=? (string-ref programaPDL i) #\U) ;Se o caractere na posição atual é igual a "U"
        (set! listaInterna (append listaInterna (list(string-ref programaPDL i)))) ;Coloca no final da listaInterna o valor achado
        (verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
      ]

      [(char=? (string-ref programaPDL i) #\*) ;Se o caractere na posição atual é igual a "*"
        (set! listaInterna (append listaInterna (list(string-ref programaPDL i)))) ;Coloca no final da listaInterna o valor achado
        (verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
      ]

      [(char=? (string-ref programaPDL i) #\;) ;Se o caractere na posição atual é igual a ";"
        (display listaInterna)

        (cond

          [(= (length listaInterna 1) 1) ;Significa que ele leu algo como: "a"


          ]

          [(= (length listaInterna 1) 2) ;Significa que ele leu algo como: "a*"

          ]
          
          [(= (length listaInterna 1) 3) ;Significa que ele leu algo como: "aUb"

          ]
        )
        
      ]
   
    )
  
)