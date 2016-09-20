;(load "proj.lisp")
;(load "proj.fas")

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
	(loop (when (> var-h2-peso 5000) (return))
		(loop (when (> var-h1-peso 9000) (return))
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
			  			(setf melhor-peso2 var-h2-peso))
			  		(progn
			  			(format t "Nao foi encontrado novo maximo: p1: ~D; p2: ~D; p3: ~D; ~%" pontos1 pontos2 pontos3)))
			  	(setf var-h1-peso (+ escala-h1 var-h1-peso)))
		(setf var-h1-peso 0)
		(setf var-h2-peso (+ escala-h2 var-h2-peso)))
	(cons pontosTotal (cons melhor-peso1 melhor-peso2))))
	
	
(defun improve2 ()
	(let* ((pontos1 0)
		   (pontos2 0) 
		   (pontos3 0)
		   (soma 0)
		   (melhor-peso3 0)
		   (melhor-peso4 0)
		   (pontosTotal 0)
		   (escala-h3 50)
		   (escala-h4 100))
	(loop (when (> var-h4-peso 5000) (return))
		(loop (when (> var-h3-peso 9000) (return))
			  (progn
			    (format t "teste com peso 3: ~D; ~%" var-h3-peso)
			    (format t "teste com peso 4: ~D; " var-h4-peso)
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
			  			(setf melhor-peso3 var-h3-peso)
			  			(setf melhor-peso4 var-h4-peso))
			  		(progn
			  			(format t "Nao foi encontrado novo maximo: p1: ~D; p2: ~D; p3: ~D; ~%" pontos1 pontos2 pontos3)))
			  	(setf var-h3-peso (+ escala-h3 var-h3-peso)))
		(setf var-h3-peso 0)
		(setf var-h4-peso (+ escala-h4 var-h4-peso)))
	(cons pontosTotal (cons melhor-peso3 melhor-peso4))))
	
	
(defun improve3 ()
	(let* ((lista-pesos (list var-h1-peso var-h2-peso var-h3-peso var-h4-peso))
		   (pontos1 0)
		   (pontos2 0) 
		   (pontos3 0)
		   (soma 0)
		   (melhor-peso1 0)
		   (melhor-peso2 0)
		   (melhor-peso3 0)
		   (melhor-peso4 0)
		   (topo-h1 3000)
		   (topo-h2 3000)
		   (topo-h3 3000)
		   (topo-h4 3000)
		   (pontosTotal 0)
		   (escala 500)
		   (n 0))
		(loop
			(if (> n 3) (setf n 0))
			(loop for i in lista-pesos do
			 	(progn
	 				(loop 
	 					(when (< i 0) (return))
	 					(format t "list inicial: ")
	 					(print lista-pesos)(format t "~%")
	 					(setf pontos1 (executa-jogadas-sem-output ge1 (procura-best gt1 50pecas)))
					  	(format t "pontos1: ~D; " pontos1)
					  	(setf pontos2 (executa-jogadas-sem-output ge2 (procura-best gt2 50pecas)))
					  	(format t "pontos2: ~D; " pontos2)
						(setf pontos3 (executa-jogadas-sem-output ge3 (procura-best gt3 50pecas)))
					  	(format t "pontos3: ~D; ~%" pontos3)
					  	(setf soma (+ pontos1 pontos2 pontos3))
	 					(if (> soma pontosTotal)
					  		(progn
								(setf pontosTotal soma)
								(cond ((= 0 n)
									(progn
							  			(setf melhor-peso1 var-h1-peso)
							  			(setf var-h1-peso (- var-h1-peso escala))
							  			
							  			
							  			(if (< var-h1-peso 0) (setf var-h1-peso topo-h1))
							  			(setf (first lista-pesos) var-h1-peso)))
						  			((= 1 n)
									(progn
							  			(setf melhor-peso2 var-h2-peso)
							  			(setf var-h2-peso (- var-h2-peso escala))
							  			
							  			
							  			(if (< var-h2-peso 0) (setf var-h2-peso topo-h2))
							  			(setf (second lista-pesos) var-h2-peso)))
						  			((= 2 n)
									(progn
							  			(setf melhor-peso3 var-h3-peso)
							  			(setf var-h3-peso (- var-h3-peso escala))
							  			
							  			
							  			(if (< var-h3-peso 0) (setf var-h3-peso topo-h3))
							  			(setf (third lista-pesos) var-h3-peso)))
						  			((= 3 n)
									(progn
							  			(setf melhor-peso4 var-h4-peso)
							  			(setf var-h4-peso (- var-h4-peso escala))
							  			
							  			
							  			(if (< var-h4-peso 0) (setf var-h4-peso topo-h1))
							  			(setf (fourth lista-pesos) var-h4-peso))))
						  			(format t "   -> Novo maximo encontrado: ~D; p1: ~D; p2: ~D; p3: ~D; ~%" soma pontos1 pontos2 pontos3)
						  			(format t "list seguinte: ")
						  			(print lista-pesos)(format t "~%"))
							(progn
	  							(format t "Nao foi encontrado novo maximo: p1: ~D; p2: ~D; p3: ~D; ~%" pontos1 pontos2 pontos3)
	  							(cond ((= 0 n)
	  								(progn
	  									(setf var-h1-peso (- var-h1-peso escala))
	  									
							  			(if (< var-h1-peso 0) (setf var-h1-peso topo-h1))
							  			(setf (first lista-pesos) var-h1-peso)))
						  			((= 1 n)
						  			(progn
	  									(setf var-h2-peso (- var-h2-peso escala))
	  									
							  			(if (< var-h2-peso 0) (setf var-h2-peso topo-h2))
							  			(setf (second lista-pesos) var-h2-peso)))
						  			((= 2 n)
						  			(progn
	  									(setf var-h3-peso (- var-h3-peso escala))
	  									
							  			(if (< var-h3-peso 0) (setf var-h3-peso topo-h3))
							  			(setf (third lista-pesos) var-h3-peso)))
						  			((= 3 n)
						  			(progn
	  									(setf var-h4-peso (- var-h4-peso escala))
	  									
							  			(if (< var-h4-peso 0) (setf var-h4-peso topo-h4))
							  			(setf (fourth lista-pesos) var-h4-peso))))
							  	(format t "list seguinte: ")
						  			(print lista-pesos)(format t "~%")))))
				(incf n 1)))))
					  		