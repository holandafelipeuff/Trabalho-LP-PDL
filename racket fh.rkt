#lang racket
(require graph)

(define g (weighted-graph/directed '((a x y) (b y z))))

(define pdl "a;b")


(define (fazTransicaoGrafo programaPDL i grafo listaInterna estadoAtual verticesPossiveis j)
  (if (= (length verticesPossiveis) j)
      (void)
      (verificaPDLGrafo programaPDL i grafo '() (list-ref verticesPossiveis j))
  )
)

(define (verificaPossiveisVertices grafo estadoAtual aresta vizinhos i listaVerticesPossiveis)
  (if (= (length vizinhos) i)
      listaVerticesPossiveis
      (if (equal? (edge-weight grafo estadoAtual (list-ref vizinhos i)) (string->symbol (string aresta)))
          (verificaPossiveisVertices grafo estadoAtual aresta vizinhos (+ i 1) (append listaVerticesPossiveis (list(list-ref vizinhos i))))
          (verificaPossiveisVertices grafo estadoAtual aresta vizinhos (+ i 1) listaVerticesPossiveis)

       )
  )
)

(define (verificaPDLGrafo programaPDL i grafo listaInterna estadoAtual)
  (cond

    [(not (= (string-length programaPDL) i))
    
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

        (cond

          [(= (length listaInterna) 1) ;Significa que ele leu algo como: "a"
           (define aresta (list-ref listaInterna 0)) ;Coloco na variável "aresta" o valor q está na lista interna
           (display aresta)
           
           (define vizinhos (get-neighbors g estadoAtual)) ;Coloco na variável "vizinhos" todos os vértices que são vizinhos ao nó atual
           (display vizinhos)

           (define verticesPossiveis (verificaPossiveisVertices grafo estadoAtual aresta vizinhos 0 '()))
           (display verticesPossiveis)

           (cond 
     
             [(= (length verticesPossiveis) 0)
                (display "Temos um problema pois não há transição no vertice ")
                (display estadoAtual)
                (display "usando a transição ")
                (display aresta)
              ]

             [(not (= (length verticesPossiveis) 0))
                (fazTransicaoGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual verticesPossiveis 0)
              ]
            )
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
    ]

    [(= (string-length programaPDL) i)
         (display "DEU BOM")
     ]
  )
)