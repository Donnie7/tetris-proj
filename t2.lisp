(load "proj.lisp")

;tabuleiro vazio
(defvar gt1 (cria-tabuleiro)) 
;tabuleiro aleatório
(defvar gt2 (cria-tabuleiro-aleatorio 1 0.1))
(defvar gt3 (cria-tabuleiro))


(dotimes (coluna 2)
	(tabuleiro-preenche! gt3 0 coluna)
	(tabuleiro-preenche! gt3 1 coluna)
	(tabuleiro-preenche! gt3 2 coluna)
	(tabuleiro-preenche! gt3 3 coluna))

(dotimes (coluna 4)
	(tabuleiro-preenche! gt3 0 (+ coluna 4))
	(tabuleiro-preenche! gt3 1 (+ coluna 4))
	(tabuleiro-preenche! gt3 2 (+ coluna 4))
	(tabuleiro-preenche! gt3 3 (+ coluna 4)))

(dotimes (coluna 1)
	(tabuleiro-preenche! gt3 0 (+ coluna 9))
	(tabuleiro-preenche! gt3 1 (+ coluna 9))
	(tabuleiro-preenche! gt3 2 (+ coluna 9))
	(tabuleiro-preenche! gt3 3 (+ coluna 9)))
	

(setf 30pecas (random-pecas 30))
(setf 50pecas (random-pecas 50))

(defvar ge1 (make-estado :tabuleiro gt1 :pecas-por-colocar 50pecas))

(defvar gp1 
	(make-problema :estado-inicial ge1
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))
(defvar ge2 (make-estado :tabuleiro gt2 :pecas-por-colocar 50pecas))
(defvar gp2
	(make-problema :estado-inicial ge2
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))

(defvar ge3 (make-estado :tabuleiro gt3 :pecas-por-colocar 50pecas))
(defvar gp3
	(make-problema :estado-inicial ge3
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))
			
			
(defun executa-jogadas-sem-output (estado-inicial lista-accoes)
	(let ((estado estado-inicial))
		(do () ((or (estado-final-p estado) (null lista-accoes)))
			(setf estado (resultado estado (first lista-accoes)))
			(setf lista-accoes (rest lista-accoes)))
		(estado-pontos estado)))	 
		
		
(defun improve ()
	(let* ((pontos1 0)
		   (pontos2 0) 
		   (pontos3 0)
		   (soma 0)
		   (melhor-peso1 0)
		   (melhor-peso2 0)
		   (pontosTotal 0)
		   (escala-h1 50)
		   (escala-h2 100))
	(loop
		(when (> var-h1-peso 8000) (return))
	(loop
		(when (> var-h2-peso 10000) (return))
		  (progn
		    (format t "teste com peso 1: ~D; ~%" var-h1-peso)
		    (format t "teste com peso 2: ~D; " var-h2-peso)
		  	(setf pontos1 (executa-jogadas-sem-output ge1 (procura-best gt1 50pecas)))
		  	(format t "pontos1: ~D; " pontos1)
		  	(setf pontos2 (executa-jogadas-sem-output ge2 (procura-best gt2 50pecas)))
		  	(format t "pontos2: ~D; " pontos2)
			(setf pontos3 (executa-jogadas-sem-output ge3 (procura-best gt3 50pecas)))
		  	(format t "pontos3: ~D; ~%" pontos3)
		  	(setf soma (+ pontos1 pontos2 pontos3)))
		  	(if (> soma pontosTotal)
		  		(progn
		  			(format t "   -> Novo maximo encontrado: ~D; p1: ~D; p2: ~D; p3: ~D; ~%" soma pontos1 pontos2 pontos3)
		  			(setf pontosTotal soma)
		  			(setf melhor-peso1 var-h1-peso)
		  			(setf melhor-peso2 var-h2-peso)
		  			(setf var-h1-peso (+ escala-h1 var-h1-peso))
		  			(setf var-h2-peso (+ escala-h2 var-h2-peso)))
		  		(progn
		  			(format t "Nao foi encontrado novo maximo: p1: ~D; p2: ~D; p3: ~D; ~%" pontos1 pontos2 pontos3)
		  			(setf var-h1-peso (+ escala-h1 var-h1-peso))
		  			(setf var-h2-peso (+ escala-h1 var-h2-peso))))))
	(cons pontosTotal (cons melhor-peso1 melhor-peso2))))
		  	
		  	
