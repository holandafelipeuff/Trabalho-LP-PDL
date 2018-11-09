#lang racket
(require graph)

;DEFINIÇÕES PARA TESTES

;Definição de um grafo direcionado com peso nas arestas (vem do próprio Racket)
(define g (weighted-graph/directed '((a x y) (b y z)))) 

;Definição de um programa PDL (lembrando que todos os testes de programas PDL no nosso programa precisa terminar com ";"
(define pdl "a;b;")

;FUNÇÕES AUXILIARES

;Função para fazer a transição dentro do grafo
(define (fazTransicaoGrafo programaPDL i grafo listaInterna estadoAtual verticesPossiveis j)
	(cond
	
		;Caso j tenha o mesmo valor da quantidade de valores dentro dos vértices possíveis, então acabou os caminhos
		[(= (length verticesPossiveis) j)
			(void)
		]
		;Caso não, então ainda temos vértices para fazer a transição
		[(not (= (length verticesPossiveis) j))
			
			;Faz a transição chamando a função principal com o novo estadoAtual
			(verificaPDLGrafo programaPDL i grafo '() (list-ref verticesPossiveis j))
			
			;Chamo recursivamente essa função para testar os outros vértices possíveis 
			(fazTransicaoGrafo programaPDL i grafo listaInterna estadoAtual verticesPossiveis (+ j 1))
		]
	)
)

;Função específica para a transição do tipo a*, mas está incompleta ainda
(define (fazTransicaoGrafoEstrela programaPDL i grafo listaInterna estadoAtual verticesPossiveis j k) 
	(cond
		[(= (length verticesPossiveis) j)
			(void)
		]
		[(not (= (length verticesPossiveis) j))
			(verificaPDLGrafo programaPDL i grafo '() (list-ref verticesPossiveis j))
			(fazTransicaoGrafo programaPDL i grafo listaInterna estadoAtual verticesPossiveis (+ j 1))
		]
	)
)

;Função que retorna os possíveis nós que podemos chegar a partir do estadoAtual com a aresta específica
;isso a gente primeiro tem q calcular quais TODOS os vizinhos do estadoAtual independente da aresta
;tal informação é passada no argumento "vizinhos"
(define (verificaPossiveisVertices grafo estadoAtual aresta vizinhos i listaVerticesPossiveis)

	;Caso o i chegou no valor do tamanho da lista vizinhos, então não temos mais o que checar
	(if (= (length vizinhos) i)

		;e retornamos o valor uma lista com os possíveis nós
		listaVerticesPossiveis 
		
		;caso não tenha chegado no final, significa que temos que verificar se o próximo vizinho tem como chegar pela aresta específica
		(if (equal? (edge-weight grafo estadoAtual (list-ref vizinhos i)) (string->symbol (string aresta)))
			;Caso tenha, adicionamos esse vértice a listaVerticesPossiveis
			(verificaPossiveisVertices grafo estadoAtual aresta vizinhos (+ i 1) (append listaVerticesPossiveis (list(list-ref vizinhos i))))
			
			;Caso não, fazemos quase o mesmo q em cima, mas não adicionamos nada a listaVerticesPossiveis
			(verificaPossiveisVertices grafo estadoAtual aresta vizinhos (+ i 1) listaVerticesPossiveis)

		)
	)
)

;PROGRAMA PRINCIPAL
(define (verificaPDLGrafo programaPDL i grafo listaInterna estadoAtual)
	(cond
		;Pergunto se chegamos no final da string que contém o programa PDL, se não... então temos ainda o que fazer
		[(not (= (string-length programaPDL) i))
		
			(cond 
				;Aqui iremos ler caracter por caracter da string do programa e ir colocando numa listaInterna para guardar
				;os subprogramas atômicos que iremos encontrar
				
				;Se o caractere na posição atual é igual a "a"
				[(char=? (string-ref programaPDL i) #\a)
					;Coloca no final da listaInterna o valor achado
					(set! listaInterna (append listaInterna (list(string-ref programaPDL i))))
					
					;E chamamos recursivamente a função para ler o próximo caracter do programa
					(verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
				]

				;Fazemos a mesma lógica para "b", "c", "U" e "*"
				
				[(char=? (string-ref programaPDL i) #\b) 
					(set! listaInterna (append listaInterna (list(string-ref programaPDL i)))) 
					(verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
				]

                                [(char=? (string-ref programaPDL i) #\c)
					(set! listaInterna (append listaInterna (list(string-ref programaPDL i))))
					(verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
				]

				[(char=? (string-ref programaPDL i) #\U) 
					(set! listaInterna (append listaInterna (list(string-ref programaPDL i)))) 
					(verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
				]

				[(char=? (string-ref programaPDL i) #\*) 
					(set! listaInterna (append listaInterna (list(string-ref programaPDL i))))
					(verificaPDLGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual)
				]

				;Caso lermos um ";", significa que dentro da listaInterna temos um subprograma atômico que deve
				;ser tratado
				
				;Se o caractere na posição atual é igual a ";"
				[(char=? (string-ref programaPDL i) #\;)

					(cond
						
						;Se a quantidade de valores dentro da listaInterna é 1, então acabamos de ler um "a;" por exemplo
						[(= (length listaInterna) 1) 
						
							;Coloco na variável "aresta" o valor q está na lista interna
							(define aresta (list-ref listaInterna 0))
							(display aresta)
					   
							;Coloco na variável "vizinhos" todos os vértices que são vizinhos ao nó atual
							(define vizinhos (get-neighbors grafo estadoAtual))
							(display vizinhos)

							;Acho todos os verticesPossiveis que podemos chegar com o estadoAtual e a aresta lida do programa PDL
							(define verticesPossiveis (verificaPossiveisVertices grafo estadoAtual aresta vizinhos 0 '()))
							(display verticesPossiveis)

							(cond 
								
								;Se a lista VerticesPossiveis estiver vazia, deu ruim
								[(= (length verticesPossiveis) 0)
									(display "\nTemos um problema pois não há transição no vertice ")
									(display estadoAtual)
									(display " usando a transição ")
									(display aresta)
								]
								
								;Caso não, fazemos a transição
								[(not (= (length verticesPossiveis) 0))
									(fazTransicaoGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual verticesPossiveis 0)
								]
							)
						]

						;Se a quantidade de valores dentro da listaInterna é 2, então acabamos de ler um "a*;" por exemplo
						[(= (length listaInterna) 2) 
						
							;Coloco na variável "aresta" o valor q está na lista interna
                            (define aresta (list-ref listaInterna 0))
						    (display aresta)
					   
							;Coloco na variável "vizinhos" todos os vértices que são vizinhos ao nó atual
							(define vizinhos (get-neighbors grafo estadoAtual)) ;Coloco na variável "vizinhos" todos os vértices que são vizinhos ao nó atual
							(display vizinhos)
							
							;Acho todos os verticesPossiveis que podemos chegar com o estadoAtual e a aresta lida do programa PDL
							(define verticesPossiveis (verificaPossiveisVertices grafo estadoAtual aresta vizinhos 0 '()))
							(display verticesPossiveis)

                            (cond 
								;Se a lista VerticesPossiveis estiver vazia, deu ruim
								[(= (length verticesPossiveis) 0)
									(display "\nTemos um problema pois não há transição no vertice ")
									(display estadoAtual)
									(display " usando a transição ")
									(display aresta)
                                    (display "* ")
								]
								;Caso não, fazemos a transição
								[(not (= (length verticesPossiveis) 0))
									(fazTransicaoGrafoEstrela programaPDL (+ i 1) grafo listaInterna estadoAtual verticesPossiveis 0 10)
								]
							)
						]
					  
						;Se a quantidade de valores dentro da listaInterna é 3, então acabamos de ler um "aUb;" por exemplo	
						[(= (length listaInterna) 3)
						
							;Coloco as arestas em suas respectivas variaveis
							(define aresta1 (list-ref listaInterna 0))
							(define aresta2 (list-ref listaInterna 2))
							   
							;Vejo todos os vizinhos do estadoAtual independente de aresta
							(define vizinhos (get-neighbors grafo estadoAtual)) ;Coloco na variável "vizinhos" todos os vértices que são vizinhos ao nó atual
							(display vizinhos)
		
							;Guardo os vizinhos possiveis da aresta1
							(define verticesPossiveis1 (verificaPossiveisVertices grafo estadoAtual aresta1 vizinhos 0 '()))
							(display verticesPossiveis1)
	
							;Guardo os vizinhos possiveis da aresta2
							(define verticesPossiveis2 (verificaPossiveisVertices grafo estadoAtual aresta2 vizinhos 0 '()))
							(display verticesPossiveis2)

							(cond
								;se ambas as listas estão vazias, então deu ruim
								[(and (= (length verticesPossiveis1) 0) (= (length verticesPossiveis2) 0))
									(display "\nTemos um problema pois não há transição no vertice ")
									(display estadoAtual)
									(display " usando a transição ")
									(display aresta1)
									(display " U ")
									(display aresta2)
								]
								
								;se as duas não estão vazias, faz a transição pra cada lista de vertices possiveis
								[(and (not(= (length verticesPossiveis1) 0)) (not(= (length verticesPossiveis2) 0)))
									(fazTransicaoGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual verticesPossiveis1 0)
                                    (fazTransicaoGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual verticesPossiveis2 0) 
								]

								;abaixo é o teste pra caso só uma das listas não esteja vazia
								[(not(= (length verticesPossiveis1) 0))
									(fazTransicaoGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual verticesPossiveis1 0)  
								]
								
								[(not(= (length verticesPossiveis2) 0))
                                    (fazTransicaoGrafo programaPDL (+ i 1) grafo listaInterna estadoAtual verticesPossiveis2 0)  
								]
							)
						]
					)	
				]  
			)
		]
		
		;Se chegamos ao final da string do programa, e não chegamos a uma mensagem de erro, então conseguimos executar tudo
		;certo, logo, o grafo consegue transcrever o programa PDL em uma de suas execuções
		[(= (string-length programaPDL) i)
			;Deu Bom :D
			(display "\nDEU BOM")
		]
	)
)