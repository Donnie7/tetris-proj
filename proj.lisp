; Gabriel Calmeiro 57774
; Ricardo Afonso 71070 
; Marco Tomas 65921
; Grupo TG 14


(defstruct estado
        (pontos 0)
        pecas-por-colocar
        pecas-colocadas
        tabuleiro)

(defstruct problema
        estado-inicial
        solucao 
        accoes
        resultado
        custo-caminho)

(defun largura-matriz (peca)
	(array-dimension peca 1))

(defun altura-matriz (peca)
	(array-dimension peca 0))

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
		   (setf (aref novo-array l c) (aref tab l c))))
	  novo-array))
		
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

(defun tabuleiro-copia-linha! (tab l-orig l-dest)
	(dotimes (n (largura-matriz tab))
		(setf (aref tab l-dest n)
			  (aref tab l-orig n))))

(defun tabuleiro-remove-linha! (tab l)
	(dotimes (n (largura-matriz tab))
		(setf (aref tab l n) nil)))


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
	(let* ((e (make-estado)))
	   (setf (estado-pontos e) (estado-pontos e1))
	   (setf (estado-pecas-por-colocar e) (copy-list (estado-pecas-por-colocar e1)))
	   (setf (estado-pecas-colocadas e) (copy-list (estado-pecas-colocadas e1)))
	   (setf (estado-tabuleiro e) (copia-tabuleiro (estado-tabuleiro e1)))
   e))
	   


(defun estados-iguais-p (e1 e2)
   (let* ((res T))
      (cond ((not (= (estado-pontos e1)
     	 	      (estado-pontos e2))) (setf res nil))
	    ((not (equalp (estado-pecas-por-colocar e1)
		(estado-pecas-por-colocar e2))) (setf res nil))
	    ((not (equalp (estado-pecas-colocadas e1)
                        (estado-pecas-colocadas e2))) (setf res nil))
	    ((not (tabuleiros-iguais-p
			 (estado-tabuleiro e1)
			 (estado-tabuleiro e2))) (setf res nil)))
      res))


(defun estado-final-p (e)
	(or (tabuleiro-topo-preenchido-p (estado-tabuleiro e))
	    (null (estado-pecas-por-colocar e))))


;#######################################
;2.1.1 Funcoes do problema de procura
;#######################################
(defun solucao (e)
	(and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro e)))
		    (null (estado-pecas-por-colocar e))))

#|
(defun roda-peca (peca)
	(let* ((l (array-dimension peca 0))
	       (c (array-dimension peca 1))
	       (nova-peca (make-array (list c l) :initial-element 0)))
	 (loop for coluna from 0 below c do
		(loop for linha from 0 below l do
		    (setf (aref nova-peca coluna linha)
			  (aref peca (- (- l linha) 1) coluna))))
	nova-peca))
|#

(defun roda-peca (peca)
	(cond ((equalp peca peca-i0) peca-i1)
		  ((equalp peca peca-i1) peca-i0)
		  ((equalp peca peca-l0) peca-l1)
		  ((equalp peca peca-l1) peca-l2)
		  ((equalp peca peca-l2) peca-l3)
		  ((equalp peca peca-l3) peca-l0)
		  ((equalp peca peca-j0) peca-j1)
		  ((equalp peca peca-j1) peca-j2)
		  ((equalp peca peca-j2) peca-j3)
		  ((equalp peca peca-j3) peca-j0)
		  ((equalp peca peca-o0) peca-o0)
		  ((equalp peca peca-s0) peca-s1)
		  ((equalp peca peca-s1) peca-s0)
		  ((equalp peca peca-z0) peca-z1)
		  ((equalp peca peca-z1) peca-z0)
		  ((equalp peca peca-t0) peca-t1)
		  ((equalp peca peca-t1) peca-t2)
		  ((equalp peca peca-t2) peca-t3)
		  ((equalp peca peca-t3) peca-t0)))

(defun testa-limites-laterais (tab accao)
	(let* ((c (-(array-dimension tab 1) 1))
	       (larg-peca (largura-matriz (cdr accao))))
	  (<= (+ (car accao) (- larg-peca 1)) c)))

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


;;; peca-encaixa-p -> boolean
;;; verifica se ha colisoes entre a peca e o tabuleiro na 
;;; coluna col e 
(defun peca-encaixa-p (tab peca desce-n-linhas col)
	(let* ((alt-tab (- (altura-matriz tab) 1 desce-n-linhas))
		   (l-peca (largura-matriz peca))
		   (a-peca (altura-matriz peca))
		   (res T))
		  (dotimes (l l-peca)
		  		(if (eq res nil)
		  			(return))
		  		(dotimes (a a-peca)
		  			(if (<= (+ (- alt-tab a) (- a-peca 1)) alt-tab)
				  			(if (and (eq (aref tab (+ (- alt-tab a) (- a-peca 1)) 
				  							       (+ col l)) T)
				  					 (eq (aref peca (- a-peca a 1) l ) T))
				  				(progn
				  					(setf res nil)
				  					(return))))
		  		)
		  )
		  res))

(defun tabuleiro-preenche-peca! (tab peca l c)
	(let* ((alt-tab (altura-matriz tab))
		   (alt-peca (altura-matriz peca))
		   (larg-peca (largura-matriz peca)))
		(dotimes (li alt-peca)
		  	(dotimes (co larg-peca)
		  		(if (and (eq (aref peca li co) 'T)
		  				 (< (+ l li) alt-tab))
			  		(setf (aref tab (+ l li) (+ c co))
			  			  (aref peca li co)))
		  		))))


;;; esta funcao precisa de ser melhorada. pode ser feita 
;;; de forma muito mais eficiente com recursao
(defun tabuleiro-desce-e-desenha-peca! (tab peca c)
	(let* ((ultima-linha-valida nil)
		   (n-descidas-permitidas (- (altura-matriz tab)
		   							 (altura-matriz peca)))
		   (n 1))
		(progn
			(if (peca-encaixa-p tab peca 0 c)
				(progn
					(setf ultima-linha-valida (- (altura-matriz tab) 1)))
				(return))
			(loop
				(when (or (eq ultima-linha-valida 0)
						  (not (peca-encaixa-p tab peca n c)))
					  	(tabuleiro-preenche-peca! tab peca ultima-linha-valida c)
					  	(return))
				(decf ultima-linha-valida 1)
				(incf n 1)))))

(defun rebenta-linhas-completas (tab)
	(let* ((alt-tab (altura-matriz tab))
		   (s 0)
		   (linhas-rebentadas 0))
		(dotimes (n alt-tab)
			(if (tabuleiro-linha-completa-p tab n)
				(progn
					(tabuleiro-remove-linha! tab n)
					(incf linhas-rebentadas 1))
				(if (eq s n)
					(incf s 1)		
					(progn
						(tabuleiro-copia-linha! tab n s)
						(tabuleiro-remove-linha! tab n)
						(incf s 1))
					)
				))
		linhas-rebentadas))

(defun calcula-pontos (pontos)
	(cond ((eq pontos 0) 0)
		   ((eq pontos 1) 100)
		   ((eq pontos 2) 300)
		   ((eq pontos 3) 500)
		   ((eq pontos 4) 800)))

(defun resultado (estado accao)
	(let* ((novo-estado (copia-estado estado)))
		(progn 
			(tabuleiro-desce-e-desenha-peca! 
				(estado-tabuleiro novo-estado)
				(cdr accao)
				(car accao))
			(if (not (tabuleiro-topo-preenchido-p (estado-tabuleiro novo-estado)))
				(setf (estado-pontos novo-estado) 
						(+ (estado-pontos novo-estado) 
							(calcula-pontos(rebenta-linhas-completas (estado-tabuleiro novo-estado)))))))
		novo-estado))
					


(defun qualidade (e)
	(- (estado-pontos e)))



;(load "utils.fas")
