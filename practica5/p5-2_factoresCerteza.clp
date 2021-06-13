; [CASTELLANO]
; Practica 5.2: Factores de Certeza en CLIPS
; Asignatura: Ingenieria del Conocimiento
; Autor: Valentino Lugli (Github: @RhinoBlindado)
; Fecha: Mayo, Junio 2021

; [ENGLISH]
; Practice 5.2: Certainty Factors in CLIPS
; Course: Knowledge Engineering
; Author: Valentino Lugli (Github: @RhinoBlindado)
; Date: May, June 2021

; (FactorCerteza ?h si|no ?f) representa que ?h se ha deducido con factor de certeza ?f
;?h podrá_ser:
; - problema_starter
; - problema_bujias
; - problema_batería
; - motor_llega_gasolina
; (Evidencia ?e si|no) representa el hecho de si evidencia ?e se da
; ?e podrá ser:
; - hace_intentos_arrancar
; - hay_gasolina_en_deposito
; - encienden_las_luces
; - gira_motor

;;; convertimos cada evidencia en una afirmación sobre su factor de certeza
(defrule certeza_evidencias
    (Evidencia ?e ?r)
        =>
    (assert (FactorCerteza ?e ?r 1)) 
)
;; También podríamos considerar evidencias con una cierta
;;incertidumbre: al preguntar por la evidencia, pedir y recoger
;;directamente el grado de certeza

; Función encadenado
(deffunction encadenado (?fc_antecedente ?fc_regla)
    
    (if (> ?fc_antecedente 0) then
        (bind ?rv (* ?fc_antecedente ?fc_regla))
    else
        (bind ?rv 0) 
    )
    ?rv
)

;R1: SI el motor obtiene gasolina Y el motor gira ENTONCES problemas con las bujías con certeza 0,7
(defrule R1
    (FactorCerteza motor_llega_gasolina si ?f1)
    (FactorCerteza gira_motor si ?f2)
    (test (and (> ?f1 0) (> ?f2 0)))
        =>
    (assert (FartorCerteza problema_bujias si (encadenado (* ?f1 ?f2) 0.7)))
)

(defrule R2
    (FactorCerteza gira_motor no ?f1)
    (test (> ?f1 0))
        =>
    (assert (FactorCerteza problema_starter si (encadenado ?f1 0.9)))    
)

(defrule R3
    (FactorCerteza encienden_las_luces no ?f1)
    (test (> ?f1 0))
        =>
    (assert (FactorCerteza problema_batería si (encadenado ?f1 0.9)))
)

(defrule R4
    (FactorCerteza hay_gasolina_en_deposito si ?f1)
    (test (> ?f1 0))
        =>
    (assert (FactorCerteza motor_llega_gasolina si (encadenado ?f1 0.9)))
)

(defrule R5
    (FactorCerteza hace_intentos_arrancar si ?f1)
    (test (> ?f1 0))
        =>
    (assert (FactorCerteza problema_starter si (encadenado ?f1 -0.6)))
    (assert (FactorCerteza problema_batería si (encadenado ?f1 0.5)))
)

; Certeza de las hipótesis
(deffunction combinacion (?fc1 ?fc2)
    
    (if (and (> ?fc1 0) (> ?fc2 0) ) then
        (bind ?rv (- (+ ?fc1 ?fc2) (* ?fc1 ?fc2) ) )
    else
        (if (and (< ?fc1 0) (< ?fc2 0) )
    then
        (bind ?rv (+ (+ ?fc1 ?fc2) (* ?fc1 ?fc2) ) )
    else
        (bind ?rv (/ (+ ?fc1 ?fc2) (- 1 (min (abs ?fc1) (abs ?fc2))) )))
    )

    ?rv
)

(defrule combinar
    (declare (salience 1))
    ?f <- (FactorCerteza ?h ?r ?fc1)
    ?g <- (FactorCerteza ?h ?r ?fc2)
    (test (neq ?fc1 ?fc2))
        =>
    (retract ?f ?g)
    (assert (FactorCerteza ?h ?r (combinacion ?fc1 ?fc2))) 
)

; Certeza de las hipótesis
(defrule combinar_signo
    (declare (salience 2))
    (FactorCerteza ?h si ?fc1)
    (FactorCerteza ?h no ?fc2)
       =>
    (assert (Certeza ?h (- ?fc1 ?fc2)))     
)

(defrule getCertainty
    (declare (salience -1))
    (FactorCerteza ?h ?ans ?value)
    =>
    (assert (Certeza ?h ?value))
)

; Preguntar

(defrule startUp
        =>
    (printout t crlf crlf"--- Practica 5.2: Factores de Certeza ---" crlf)
    (printout t crlf ">>Tu coche tiene problemas.")
    (assert (startQuestions))
)

(defrule firstQuestion
    ?f <- (startQuestions)
        =>
    (printout t crlf ">>Hace intentos para arrancar?" crlf ">>(Si | No)" crlf ">")
    (assert (Evidencia  hace_intentos_arrancar (lowcase(read))))
    (assert (secondQ))
    (retract ?f)
)

(defrule secondQuestion
    ?f <- (secondQ)
        =>
    (printout t crlf ">>Tiene gasolina en el deposito?" crlf ">>(Si | No)" crlf ">")
    (assert (Evidencia hay_gasolina_en_deposito (lowcase(read))))
    (assert (thirdQ))
    (retract ?f)
)

(defrule thirdQuestion
    ?f <- (thirdQ)
        =>
    (printout t crlf ">>Encienden las luces?" crlf ">>(Si | No)" crlf ">")
    (assert (Evidencia gira_motor (lowcase(read))))
    (retract ?f)
)

(defrule giveAnswer
    (salience -10)
    (Certeza ?problem ?value)
    (not (Certeza ? ?value2&:(>= ?value2 ?value)))
        =>
    (printout t "Deducido: Problema es " ?problem " con certeza " ?value crlf)
)