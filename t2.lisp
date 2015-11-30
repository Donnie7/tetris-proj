(load "proj.lisp")

;tabuleiro vazio
(defvar gt1 (cria-tabuleiro)) 

;tabuleiro cheio
(defvar gt2 (cria-tabuleiro))

;tabuleiro aleatório
(defvar gt3 (cria-tabuleiro-aleatorio))
(defvar gt4 (cria-tabuleiro))


(dotimes (l 18)
	(dotimes (coluna 10)
		(tabuleiro-preenche! gt2 l coluna)))



(defvar ge1 (make-estado :tabuleiro gt1 
						:pecas-por-colocar (random-pecas 10)))




(defvar gp1
	(make-problema :estado-inicial (make-estado :tabuleiro t1 :pecas-por-colocar '(i j t))
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))

(defvar gp2
	(make-problema :estado-inicial (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l o i i l i))
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))

(defvar e1 (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l t i)))
(defvar ge3 (make-estado :tabuleiro gt3 :pecas-por-colocar '(i o j l t i)))
(defvar gt4 (cria-tabuleiro)) 

(dotimes (coluna 2)
	(tabuleiro-preenche! gt4 0 coluna)
	(tabuleiro-preenche! gt4 1 coluna)
	(tabuleiro-preenche! gt4 2 coluna)
	(tabuleiro-preenche! gt4 3 coluna))

(dotimes (coluna 4)
	(tabuleiro-preenche! gt4 0 (+ coluna 4))
	(tabuleiro-preenche! gt4 1 (+ coluna 4))
	(tabuleiro-preenche! gt4 2 (+ coluna 4))
	(tabuleiro-preenche! gt4 3 (+ coluna 4)))

(dotimes (coluna 1)
	(tabuleiro-preenche! gt4 0 (+ coluna 9))
	(tabuleiro-preenche! gt4 1 (+ coluna 9))
	(tabuleiro-preenche! gt4 2 (+ coluna 9))
	(tabuleiro-preenche! gt4 3 (+ coluna 9)))



(defvar ge4 (make-estado :tabuleiro gt4 :pecas-por-colocar '(i z s t l o i z s z o o i t t z l z s t l o i z s z o o z s j j z l s o o i j z)))
(defvar gp4
	(make-problema :estado-inicial (make-estado :tabuleiro gt4 :pecas-por-colocar '(i z s t l o i z s z o o i t t z l z s t l o i z s z o o z s j j z l s o o i j z))			
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))

(defvar gt5 (cria-tabuleiro))
(dotimes (coluna 10)
	(tabuleiro-preenche! gt5 0 coluna )
	(tabuleiro-preenche! gt5 1 coluna )
	(tabuleiro-preenche! gt5 2 coluna )
	(tabuleiro-preenche! gt5 3 coluna )
	(tabuleiro-preenche! gt5 4 coluna )
	(tabuleiro-preenche! gt5 5 coluna )
	(tabuleiro-preenche! gt5 6 coluna )
	(tabuleiro-preenche! gt5 7 coluna )
	(tabuleiro-preenche! gt5 8 coluna )
	(tabuleiro-preenche! gt5 9 coluna )
	(tabuleiro-preenche! gt5 10 coluna )
	(tabuleiro-preenche! gt5 11 coluna )
	(tabuleiro-preenche! gt5 12 coluna )
	(tabuleiro-preenche! gt5 13 coluna )
	(tabuleiro-preenche! gt5 14 coluna )
	(tabuleiro-preenche! gt5 15 coluna )
	(tabuleiro-preenche! gt5 16 coluna ))

(defvar gp5
	(make-problema :estado-inicial (make-estado :tabuleiro gt5 :pecas-por-colocar '(o))
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))

