(load "proj.lisp")



(defvar gt1 (cria-tabuleiro)) 

(dotimes (coluna 10)
	(tabuleiro-preenche! gt1 1 coluna )
	(tabuleiro-preenche! gt1 3 coluna )
	(tabuleiro-preenche! gt1 5 coluna ))

(defvar ge1 (make-estado :tabuleiro gt1 :pecas-por-colocar '(i z s )))
