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

; ##########################
; # EJERCICIO
; ##########################

;   - Preguntar por las posibles evidencias:
;   - Añadir el resto de las reglas
;   - Tras razonar quedarse con la hipótesis con mayor certeza y
;   - Añadir o modificar las reglas para que el sistema explique el por qué de las afirmaciones

; (FactorCerteza ?h si|no ?f) representa que ?h se ha deducido con factor de certeza ?f
;?h podrá_ser:
; - problema_starter
; - problema_bujias
; - problema_batería
; (Evidencia ?e si|no) representa el hecho de si evidencia ?e se da
; ?e podrá ser:
; - hace_intentos_arrancar
; - hay_gasolina_en_deposito
; - encienden_las_luces
; - gira_motor
; - motor_llega_gasolina

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

;   R1: SI el motor obtiene gasolina Y el motor gira ENTONCES problemas con las bujías con certeza 0,7
(defrule R1
    (FactorCerteza motor_llega_gasolina si ?f1)
    (FactorCerteza gira_motor si ?f2)
    (test (and (> ?f1 0) (> ?f2 0)))
        =>
    (bind ?expl (str-cat "motor obtiene gasolina y el motor gira"))
    (assert (FartorCerteza problema_bujias si (encadenado (* ?f1 ?f2) 0.7)))
)

; ##########################
; # EJERCICIO
; ##########################

;   - Añadir el resto de las reglas
;   - Añadir o modificar las reglas para que el sistema explique el por qué de las afirmaciones

;   R2: SI NO gira el motor ENTONCES problema con el starter con certeza 0,8
(defrule R2
    (FactorCerteza gira_motor no ?f1)
    (test (> ?f1 0))
        =>
    (bind ?expl (str-cat "no gira el motor"))
    (assert (FactorCerteza problema_starter si (encadenado ?f1 0.8) ?expl))    
)

;   R3: SI NO encienden las luces ENTONCES problemas con la batería con certeza 0,9
(defrule R3
    (FactorCerteza encienden_las_luces no ?f1)
    (test (> ?f1 0))
        =>
    (bind ?expl (str-cat "no encienden las luces"))
    (assert (FactorCerteza problema_bateria si (encadenado ?f1 0.9) ?expl))
)

;   R4: SI hay gasolina en el deposito ENTONCES el motor obtiene gasolina con certeza 0,9
(defrule R4
    (FactorCerteza hay_gasolina_en_deposito si ?f1)
    (test (> ?f1 0))
        =>
    (assert (FactorCerteza motor_llega_gasolina si (encadenado ?f1 0.9)))
)

;   R5: SI hace intentos de arrancar ENTONCES problema con el starter con certeza -0,6
;   R6: SI hace intentos de arrancar ENTONCES problema con la batería 0,5
(defrule R5
    (FactorCerteza hace_intentos_arrancar si ?f1)
    (test (> ?f1 0))
        =>
    (bind ?expl (str-cat "hace intentos de arrancar"))
    (assert (FactorCerteza problema_starter si (encadenado ?f1 -0.6) ?expl))
    (assert (FactorCerteza problema_bateria si (encadenado ?f1 0.5) ?expl))
)


(deftemplate Certeza
    (field problema)
    (field valor)
    (field explanation)    
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
    ?f <- (FactorCerteza ?h ?r ?fc1 ?expl1)
    ?g <- (FactorCerteza ?h ?r ?fc2 ?expl2)
    (test (neq ?fc1 ?fc2))
        =>
    (retract ?f ?g)
    (bind ?expl3 (str-cat ?expl1 ", " ?expl2))
    (assert (FactorCerteza ?h ?r (combinacion ?fc1 ?fc2) ?expl3)) 
)

; Certeza de las hipótesis

(defrule getCertainty
    (declare (salience -1))
    (FactorCerteza ?h ?ans ?value ?expl)
    =>
    (assert (Certeza (problema ?h) (valor ?value) (explanation ?expl)))
)

; ##########################
; # EJERCICIO
; ##########################

;   - Preguntar por las posibles evidencias:

(deffacts validAnswers
    (validAns si no)
)

(defrule startUp
        =>
    (printout t crlf crlf"--- Practica 5.2: Factores de Certeza ---" crlf)
    (printout t crlf ">>Tu coche tiene problemas.")
    (assert (question hace_intentos_arrancar))
)

(defrule firstQuestion
    ?f <- (question hace_intentos_arrancar)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf  ">>Hace intentos para arrancar?" crlf ">>(Si | No)" crlf ">")
    (assert (Evidencia  hace_intentos_arrancar (lowcase(read))))
    (assert (question hay_gasolina_en_deposito))
    (retract ?f)
)

(defrule secondQuestion
    ?f <- (question hay_gasolina_en_deposito)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf  ">>Tiene gasolina en el deposito?" crlf ">>(Si | No)" crlf ">")
    (assert (Evidencia hay_gasolina_en_deposito (lowcase(read))))
    (assert (question encienden_las_luces))
    (retract ?f)
)

(defrule thirdQuestion
    ?f <- (question encienden_las_luces)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf  ">>Encienden las luces?" crlf ">>(Si | No)" crlf ">")
    (assert (Evidencia encienden_las_luces (lowcase(read))))
    (assert (question gira_motor))
    (retract ?f)
)

(defrule fourthQuestion
    ?f <- (question gira_motor)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf  ">>Gira el motor?" crlf ">>(Si | No)" crlf ">")
    (assert (Evidencia gira_motor (lowcase(read))))
    (retract ?f)
    (assert (razonar))
)

; ##########################
; # EJERCICIO
; ##########################

(deffacts prettyName
    (prettyName problema_bateria "la bateria")
    (prettyName problema_starter "el starter")
    (prettyName problema_bujias "las bujias")
)

;   - Tras razonar quedarse con la hipótesis con mayor certeza.
(defrule giveAnswer
    (razonar)
    (Certeza (problema ?name1) (valor ?value1) (explanation ?expl))
    (prettyName ?name1 ?prettyName)
    (not (Certeza (valor ?value2&:(> ?value2 ?value1))))
        =>
    (printout t crlf "### RESPUESTA ###" crlf ">> El problema puede ser con " ?prettyName " con una certeza del " (* ?value1 100) "% porque: " ?expl "." crlf)
)

;   Reglas de verificación de correctitud de las respuestas.
;   - Verificar que en las preguntas textuales se responde acordemente.
(defrule checkAns
    (declare (salience 90))

    ; Leer cualquier regla de respuesta.
    ?rule <- (Evidencia ?type ?ans)

    ; Si no encuentra que la respuesta ?ans corresponde con alguna de las respuestas validas, disparar la regla.
    (not (validAns $? ?ans $?))
    
        =>
    ; Imprimir mensaje, borrar la regla de la respuesta y retornar a la pregunta.
    (printout t ">>Perdona, pero '"?ans "' no es una respuesta valida. Vuelve a responder." crlf)
    (retract ?rule)
    (assert (question ?type))
)