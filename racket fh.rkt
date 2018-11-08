#lang racket

(require graph)

(define g (weighted-graph/directed '((a x y) (b y z))))

(define pdl "a;b")

(define (verificaPossiveisVertices grafo estadoAtual aresta vizinhos i listaVerticesPossiveis)
  (if (= (length vizinhos) i)
      (listaVerticesPossiveis)
      (
         (if (= (edge-weight grafo estadoAtual (lista-ref vizinhos i)) aresta)
             (set! listaVerticesPossiveis (append listaVerticesPossiveis (list(lista-ref vizinhos i))))
         )
       )
  )
  
  

)

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

          [(= (length listaInterna) 1) ;Significa que ele leu algo como: "a"
           (define aresta (list-ref listaInterna 0)) ;Coloco na variável "aresta" o valor q está na lista interna
           
           (define vizinhos (get-neighbors g estadoAtual)) ;Coloco na variável "vizinhos" todos os vértices que são vizinhos ao nó atual
           (display vizinhos)

           (define verticesPossiveis (verificaPossiveisVertices (grafo estadoAtual aresta vizinhos 0 '())))
          ]

          [(= (length listaInterna) 2) ;Significa que ele leu algo como: "a*"
           (define aresta (list-ref listaInterna 0))
           (display "wololo")
          ]
          
          [(= (length listaInterna) 3) ;Significa que ele leu algo como: "aUb"
           (define aresta1 (list-ref listaInterna 0))
           (define aresta2 (list-ref listaInterna 2))
           (display "wololo")
          ]
        )
        
      ]
   
    )
  
)