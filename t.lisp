
(load "proj.lisp")
(load "utils.lisp")

;;; definicao das configuracoes possiveis para cada peca
;;peca i 
(defconstant peca-i0 (make-array (list 4 1) :initial-element T))
(defconstant peca-i1 (make-array (list 1 4) :initial-element T))
;;peca l
(defconstant peca-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil))))
(defconstant peca-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T))))
(defconstant peca-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T))))
(defconstant peca-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T))))
;;peca j
(defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T))))
(defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil))))
(defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T))))
(defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T))))
;;peca o
(defconstant peca-o0 (make-array (list 2 2) :initial-element T))
;;peca s
(defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T))))
(defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil))))
;;peca z
(defconstant peca-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil))))
(defconstant peca-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T))))
;;peca t
(defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil))))
(defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil))))
(defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T))))
(defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))

(setf a (cria-accao 5 peca-l0))
(setf b (cria-accao 5 peca-o0))

(setf c (accao-coluna a))
(setf d (accao-peca a))

(setf e (cria-tabuleiro))
(setf f (cria-tabuleiro))

;s e uma copia de f
(setf s (copia-tabuleiro f))

;arr e um array simples 1A
(setf arr (make-array 5 :initial-contents '(0 1 2 3 4)))





;;############################################
;;############## TESTES ######################

;TESTE 1
;antes de alterar f, tabuleiro-preenchido-p devera devolver 
;nil
(setf t1 (eq (tabuleiro-preenchido-p f 2 4) nil))

;TESTE 2
;altera a posicao 2 4 de f para T
(setf (aref f 2 4) T)
;depois da posicao de f alterada devera retornar T
(setf t2 (eq (tabuleiro-preenchido-p f 2 4) T))

;TESTE 3
;valida a altura de todas as colunas
(setf t3-1 (= (tabuleiro-altura-coluna f 0) 0))
(setf t3-2 (= (tabuleiro-altura-coluna f 1) 0))
(setf t3-3 (= (tabuleiro-altura-coluna f 4) 16))
(setf t3-4 (= (tabuleiro-altura-coluna f 9) 0))

;TESTE 4
;valida a altura depois de alteradas algumas posicoes no 
;tabuleiro de vazias para preenchidas
(setf (aref f 0 0) T)
(setf (aref f 9 9) T)
(setf t4-1 (= (tabuleiro-altura-coluna f 0) 18))
(setf t4-2 (= (tabuleiro-altura-coluna f 9) 9))
(setf (aref f 17 9) T)
(setf t4-3 (= (tabuleiro-altura-coluna f 9) 9))

;TEST 5
(setf f1 (cria-tabuleiro))
(setf (aref f1 0 0) T)
(setf (aref f1 0 1) T)
(setf (aref f1 0 2) T)
(setf (aref f1 0 3) T)
(setf (aref f1 0 4) T)
(setf (aref f1 0 5) T)
(setf (aref f1 0 6) T)
(setf (aref f1 0 7) T)
(setf (aref f1 0 8) T)
(setf (aref f1 0 9) T)

(setf (aref f1 2 0) T)
(setf (aref f1 2 1) T)
(setf (aref f1 2 2) T)
(setf (aref f1 2 3) T)
(setf (aref f1 2 4) T)
(setf (aref f1 2 5) T)
(setf (aref f1 2 6) T)
(setf (aref f1 2 7) T)
(setf (aref f1 2 8) T)
(setf (aref f1 2 9) T)

(setf (aref f1 17 0) T)
(setf (aref f1 17 1) T)
(setf (aref f1 17 2) T)
(setf (aref f1 17 3) T)
(setf (aref f1 17 4) T)
(setf (aref f1 17 5) T)
(setf (aref f1 17 6) T)
(setf (aref f1 17 7) T)
(setf (aref f1 17 8) T)

(setf t5-1 (eq (tabuleiro-linha-completa-p f1 0) T))
(setf t5-2 (eq (tabuleiro-linha-completa-p f1 1) nil))
(setf t5-3 (eq (tabuleiro-linha-completa-p f1 2) T))
(setf t5-4 (eq (tabuleiro-linha-completa-p f1 3) nil))
(setf t5-5 (eq (tabuleiro-linha-completa-p f1 17) nil))

; lista de testes
(setf test-list 
	(list 
t1 
t2 
t3-1
t3-2 
t3-3 
t3-4 
t4-1
t4-2
t4-3
t5-1
t5-2
t5-3
t5-4
t5-5))

; avalia testes
(defun testes ()
	(loop for x in test-list
		do (if x (print 'passed) (print 'failed)))
'DONE)

