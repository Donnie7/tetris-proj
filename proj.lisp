; Gabriel Calmeiro 57774
; Ricardo Afonso 71070 
; Marco Tomas 65921
; Grupo TG 14


(defstruct estado
        pontos
        pecas-por-colocar
        pecas-colocadas
        tabuleiro)

(defstruct problema
        estado-inicial
        solucao 
        accoes
        resultado
        custo-caminho)


;#######################################
;2.1.1 Tipo Accao
;#######################################
(defun cria-accao (int array)
	(cons int array))

(defun accao-coluna (accao)
	(car accao))

(defun accao-peca (accao)
	(cdr accao))


;#######################################
;2.1.2 Tipo Tabuleiro
;#######################################
(defun cria-tabuleiro ()
	(make-array (list 18 10)))

(defun copia-tabuleiro (tab)
	(let* ((linhas (array-dimension tab 0))
 	       (colunas (array-dimension tab 1))
	       (novo-array (make-array (list linhas colunas) :initial-element T)))
	  (dotimes (l linhas)
	      (dotimes (c colunas)
		   (setf (aref novo-array l c) (aref tab l c))))novo-array))
		
(defun tabuleiro-preenchido-p (tab l c)
	(eq (aref tab l c) 'T))

(defun tabuleiro-altura-coluna (tab c)
	(let* ((linhas (array-dimension tab 0))
	       (i linhas))
	      (dotimes (l linhas)
	          (cond ((eq (aref tab (- (- linhas 1) l) c) T)
			     (return)))
	          (decf i 1))
	  i))

(defun tabuleiro-linha-completa-p (tab l)
	(let* ((colunas (array-dimension tab 1))
	       (linha-total T))
	    (dotimes (c colunas)
		(cond ((eq (aref tab l c) nil) 
			   (setf linha-total nil)(return))))
	  linha-total))

(defun tabuleiro-preenche! (tab l c)
	(let* ((linhas (array-dimension tab 0))
	       (colunas (array-dimension tab 1)))
	      (cond ((< c 0)nil)
		    ((> c (- colunas 1))nil)
		    ((< l 0)nil) 
		    ((> l (- linhas 1))nil)
		    ((setf (aref tab l c) T)T)))) 

(defun tabuleiro-remove-linha! (tab l)
	(let* ((linhas (array-dimension tab 0))
	       (colunas (array-dimension tab 1))
	       (linha-acima (+ l 1)))
	      (loop
		   (when (> linha-acima (- linhas 1)) (return))
		   (dotimes (c colunas)
	  		(setf (aref tab l c) (aref tab linha-acima c)))
		   (incf l 1)
		   (incf linha-acima 1))
	      (dotimes (c colunas)
		   (setf (aref tab (- linhas 1) c) nil))))

(defun tabuleiro-topo-preenchido-p (tab)
        (let* ((colunas (array-dimension tab 1))
	       (linhas (array-dimension tab 0))
	       (topo-preenchido nil))
            (dotimes (c colunas)
                (cond ((eq (aref tab (- linhas 1) c) T)
                           (setf topo-preenchido T)(return))))
          topo-preenchido))	      		     

(defun tabuleiros-iguais-p (t1 t2)
	(let* ((c (* (array-dimension t1 0)
		     (array-dimension t1 1)))
	       (res T))
	      (cond
		 ( (null t2) (setf res nil))
		 
	      ((dotimes (l c)
		     (cond ((not (eq (row-major-aref t1 l)
				     (row-major-aref t2 l)))
			    (setf res nil)(return))))))
	      res))

(defun tabuleiro->array (tab)
	(copia-tabuleiro tab))

(defun array->tabuleiro (tab)
	(copia-tabuleiro tab))

;#######################################
;2.1.1 Tipo Estado
;#######################################

(defun copia-estado (e1)
	(copy-estado e1))

(defun estados-iguais-p (e1 e2)
	(equalp e1 e2))

(defun estado-final-p (e)
	(or (tabuleiro-topo-preenchido-p (estado-tabuleiro e))
	    (null (estado-pecas-por-colocar e))))

(defun solucao (e)
	(and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro e)))
		    (null (estado-pecas-por-colocar e))))


(defun roda-peca (peca)
	(let* ((l (array-dimension peca 0))
	       (c (array-dimension peca 1))
	       (nova-peca (make-array (list c l) :initial-element 0)))
	 (loop for coluna from 0 below c do
		(loop for linha from 0 below l do
		    (setf (aref nova-peca coluna linha)
			  (aref peca (- (- l linha) 1) coluna))))
	nova-peca))

(defun testa-limites-laterais (tab accao)
	(let* ((c (-(array-dimension tab 1) 1))
	       (largura-peca (array-dimension (cdr accao) 1)))
	  (<= (+ (car accao) (- largura-peca 1)) c)))

(defun accoes (e)
	(let* ((res ())
	       (peca-inicial (car (estado-pecas-por-colocar e)))
	       (peca-rodada nil)
	       (res-peca-rodada peca-inicial)
	       (larg-t (array-dimension (estado-tabuleiro e) 1))
	       (c 0))
	 (loop
	     (when (tabuleiros-iguais-p peca-inicial peca-rodada) (return))
             (dotimes (n larg-t)
                 (when (not (testa-limites-laterais (estado-tabuleiro e) (cria-accao c res-peca-rodada))) (return))
      		 (setf res (cons (cria-accao c res-peca-rodada) res))
		 (setf c (+ n 1)))
	     (setf c 0)
	     (setf peca-rodada (roda-peca res-peca-rodada))
	     (setf res-peca-rodada (roda-peca res-peca-rodada)))
	(reverse res)))


;#######################################
;2.1.1 Funcoes do problema de procura
;#######################################

(defun qualidade (e)
	(- (estado-pontos e)))

;(load "utils.fas")
