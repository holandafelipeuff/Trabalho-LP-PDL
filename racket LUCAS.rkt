#lang racket
(require graph)

;DEFINIÇÕES PARA TESTES
(define g (weighted-graph/directed '((a x y) (b y z))))
(define g2 (weighted-graph/directed '((a x x))))

(define pdl "a;b;")

;FUNÇÕES AUXILIARES
(define (fazTransicaoGrafo programaPDL i grafo listaInterna estadoAtual verticesPossiveis j)
  (cond
    [(= (length verticesPossiveis) j)
     (void)]
    [(not (= (length verticesPossiveis) j))
     (verificaPDLGrafo programaPDL i grafo '() (list-ref verticesPossiveis j))
     (fazTransicaoGrafo programaPDL i grafo listaInterna estadoAtual verticesPossiveis (+ j 1))]
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

;PROGRAMA PRINCIPAL
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
        (cond

          [(= (length listaInterna) 2)
           (define aresta (list-ref listaInterna 0))
           (display aresta)
           
           (define vizinhos (get-neighbors grafo estadoAtual))
           (display vizinhos)

           (define verticesPossiveis (verificaPossiveisVertices grafo estadoAtual aresta vizinhos 0 '()))
           (display verticesPossiveis)

           (cond
             [(= (length verticesPossiveis) 0)
                (display "\nTemos um problema pois não há transição no vertice ")
                (display estadoAtual)
                (display " usando a transição ")
                (display aresta)
              ]
            )
          ]
        )
      ]

      [(char=? (string-ref programaPDL i) #\;) ;Se o caractere na posição atual é igual a ";"

        (cond

          [(= (length listaInterna) 1) ;Significa que ele leu algo como: "a"
           (define aresta (list-ref listaInterna 0)) ;Coloco na variável "aresta" o valor q está na lista interna
           (display aresta)
           
           (define vizinhos (get-neighbors grafo estadoAtual)) ;Coloco na variável "vizinhos" todos os vértices que são vizinhos ao nó atual
           (display vizinhos)

           (define verticesPossiveis (verificaPossiveisVertices grafo estadoAtual aresta vizinhos 0 '()))
           (display verticesPossiveis)

           (cond 
     
             [(= (length verticesPossiveis) 0)
                (display "\nTemos um problema pois não há transição no vertice ")
                (display estadoAtual)
                (display " usando a transição ")
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
         (display " DEU BOM ")
     ]
  )
)