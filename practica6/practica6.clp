; [CASTELLANO]
; Practica 6: Sistema Basado en Conocimiento
; Asignatura: Ingenieria del Conocimiento
; Autor: Valentino Lugli (Github: @RhinoBlindado)
; Fecha: Mayo, Junio 2021

; [ENGLISH]
; Practice 6: Knowledge-Based System
; Course: Knowledge Engineering
; Author: Valentino Lugli (Github: @RhinoBlindado)
; Date: May, June 2021

; ##########################
; # INICIO MODULO ALPHA
; ##########################

;   HECHOS

;   Registro de las ramas de Ingeniería Informática
(deftemplate branch
    (field id)
    (field name)
)

;   Definición de las ramas y su nombre completo.
(deffacts defBranches
    (branch (id CSI) (name "Computacion y Sistemas Inteligentes"))
    (branch (id IS) (name "Ingenieria del Software"))
    (branch (id SI) (name "Sistemas de Informacion"))
    (branch (id IC) (name "Ingenieria de Computadores"))
    (branch (id TI) (name "Tecnologias de la Informacion"))
)

;   Definición de preguntas a realizar.
(deffacts questions
    (question hardware)
    (question programar)
    (question matematicas)
    (question trabajo)
    (question hardware)
    (question promedio)
)

;   Registro de las preguntas y las respuestas válidas que poseen.
(deftemplate validAnswers
    (field questionName)
    (multifield validAns)
)

;   Rellenar las preguntas existentes y su respuestas válidas. 
(deffacts fillValidAnswers
    (validAnswers (questionName hardware)
        (validAns "si" "no" "me da igual")
    )

    (validAnswers (questionName programar)
        (validAns "si" "no" "me da igual")
    )

    (validAnswers (questionName promedio)
        (validAns "alto" "medio" "bajo")
    )

    (validAnswers (questionName matematicas)
        (validAns "si" "no" "me da igual")
    )

    (validAnswers (questionName trabajo)
        (validAns "mucho" "medio" "poco")
    )
)

;   Registro de que rama recomendar en relación a las distintas preguntas; con más de una respuesta válida.
(deftemplate recommendationAlpha
    (field branchID)
    (multifield likesHardware)
    (multifield likesToProgram)
    (multifield howMuchWork)
    (multifield averageGrade)
    (multifield likesMath)
    (field explanation)
    (field author)
)

;   Definición propiamente de los registros para las ramas de Ingeniería Informática.
(deffacts branchRecc
    (recommendationAlpha (branchID CSI)
        (likesHardware  "no" "me da igual")
        (likesToProgram "si") 
        (howMuchWork "") 
        (averageGrade "medio" "alto") 
        (likesMath "si" "me da igual")
        (explanation "Esto es debido a que te gusta programar, tienes afinidad por las matematicas, el trabajo arduo y esto se refleja en tu promedio.") 
        (author "Valentino Lugli")
    )

    (recommendationAlpha (branchID IC)
        (likesHardware "si" "me da igual")
        (likesToProgram "no" "si" "me da igual") 
        (howMuchWork "bajo" "medio" "alto") 
        (averageGrade "alto" "medio" "bajo") 
        (likesMath "si" "no" "me da igual")
        (explanation "Esto es debido a que te gusta el hardware mas que todo, lo cual rapidamente me permite realizarte la recomendacion.") 
        (author "Valentino Lugli")
    )

    (recommendationAlpha (branchID IS)
        (likesHardware "no" "me da igual")
        (likesToProgram "si") 
        (howMuchWork "mucho") 
        (averageGrade "medio") 
        (likesMath "si" "no" "me da igual")
        (explanation "Esto es debido a que te gusta programar, te gusta trabajar y/o posees afinidad por las matematicas y un promedio que debe ser medio.") 
        (author "Valentino Lugli")
    )

    (recommendationAlpha (branchID SI)
        (likesHardware "no" "me da igual")
        (likesToProgram "si" "me da igual") 
        (howMuchWork "medio") 
        (averageGrade "alto" "medio" "bajo") 
        (likesMath "no" "me da igual")
        (explanation "Esto es debido a que posees un gusto por programar, trabajas de manera moderada y prefieres no tocar muchos las mates.") 
        (author "Valentino Lugli")
    )

    (recommendationAlpha (branchID TI)
        (likesHardware "no" "me da igual")
        (likesToProgram "no" "me da igual") 
        (howMuchWork "poco" "medio") 
        (averageGrade "medio" "bajo") 
        (likesMath "no" "me da igual" "si")
        (explanation "Esto es debido a que prefieres de carga de trabajo bajas, prefieres alejarte un poco de la programacion y las mates y tratar con informacion de otras maneras.") 
        (author "Valentino Lugli")
    )
)

;   REGLAS

; Regla inicial.
(defrule startUpAlpha
    (declare (salience 100))
    (module ALPHA)
        =>
    (printout t "-- RECOMENDACIONES SOBRE RAMAS --" crlf crlf)
    ; Se usa este hecho para evitar que se dispare una sola recomendacion.
    (assert (deduction))
)

; Reglas que obtienen informacion del usuario.
(defrule hardwareQuestion
    (deduction)
    (module ALPHA)
    ?rule <- (question hardware)
        =>
    (retract ?rule)
    (printout t crlf "--- PREGUNTA ---" crlf ">>Te gusta el hardware?" crlf ">>(Si | No | Me da igual)" crlf ">")
    ; Leer la entrada con espacios y convertirla a minuscula.
    (assert(answer hardware (lowcase (readline))))
)

(defrule programmingQuestion
    (deduction)
    (module ALPHA)
    ?rule <- (question programar)
        =>
    (retract ?rule)
    (printout t crlf "--- PREGUNTA ---" crlf ">>Te gusta programar?" crlf ">>(Si | No | Me da igual)" crlf ">")
    (assert(answer programar (lowcase (readline))))
)

(defrule mathQuestion
    (deduction)
    (module ALPHA)
    ?rule <- (question matematicas)
        =>
    (retract ?rule)
    (printout t crlf "--- PREGUNTA ---" crlf ">>Te gustan las matematicas?" crlf ">>(Si | No | Me da igual)" crlf ">")
    (assert(answer matematicas (lowcase(readline))))
)

(defrule workQuestion
    (deduction)
    (module ALPHA)
    (answer matematicas "no")
    ?rule <- (question trabajo)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf ">>Que tanto trabajas?" crlf ">>(Mucho | Medio | Poco)" crlf ">")
    (assert(answer trabajo (lowcase(readline))))
    (retract ?rule)
    (assert (module ALPHA))
)

(defrule gradesQuestion
    (deduction)
    (module ALPHA)
    (answer matematicas "si"|"me da igual")
    ?rule <- (question promedio)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf ">>Cual es tu promedio?" crlf ">>(Numero fraccional entre 5 y 10)" crlf ">")
    ; Primero se crea este hecho para convertir de número a un valor discreto.
    (assert(answerNum promedio (read)))
    (retract ?rule)
    (assert (module ALPHA))
)

; Convertir promedio a valor discreto.
(defrule gradeQuestionConvertAnswer
    (module ALPHA)  
    ?rule <- (answerNum promedio ?avg)
    ; Comprobar que el número está entre 5 y 10
    (test ( and (>= ?avg 5) (<= ?avg 10) ) )        
        =>
    (retract ?rule)
    ; Si está entre 5 y 6.99; es promedio bajo. Entre 7 y 8.99 es promedio medio y de 9 en adelante es promedio alto.
    (if (and (>= ?avg 5)(< ?avg 6.99) ) then 
        ( assert(answer promedio "bajo") ) 
    else (if (and (>= ?avg 7) (< ?avg 8.99) ) then 
        ( assert(answer promedio "medio") )  
    else ( assert (answer promedio "alto") ) ) 
    )
)

; Reglas de verificación de correctitud de las respuestas.
;   - Verificar que en las preguntas textuales se responde acordemente.
(defrule checkAns
    (declare (salience 90))
    (module ALPHA)

    ; Leer cualquier regla de respuesta.
    ?rule <- (answer ?type ?ans)

    ; Si no encuentra que la respuesta ?ans corresponde con alguna de las respuestas validas, disparar la regla.
    (not (validAnswers (questionName ?type) (validAns $? ?ans $?)))
    
        =>
    ; Imprimir mensaje, borrar la regla de la respuesta y retornar a la pregunta.
    (printout t ">>Perdona, pero '"?ans "' no es una respuesta valida sobre " ?type ". Vuelve a responder." crlf)
    (retract ?rule)
    (assert (question ?type))
)

;   - Verificar en la pregunta de promedio que se responde correctamente.
(defrule checkNumAns
    (declare (salience 90))
    (module ALPHA)

    ?rule <- (answerNum ?type ?ans)
    ; Si el numero es menos de 5 o mayor que 10 o diferente de 0...
    (test ( or (< ?ans 5) (> ?ans 10) ) )        
        =>
    ; ... Imprimir el mensaje y retornar a la pregunta.
    (printout t ">>Perdona, pero '"?ans "' no es una respuesta valida sobre " ?type ". Vuelve a responder." crlf)
    (retract ?rule)
    (assert (question ?type))
)

; Reglas de selección de rama
;   - Estas reglas capturan las diferentes maneras que un usuario puede responder, por ejemplo, solamente se puede responder a 
;     la primera pregunta y esto dispare una respuesta.
(defrule reccomender1
    (declare (salience 50))
    (module ALPHA)
    ?rule <- (deduction)

    (answer hardware "si")

    (recommendationAlpha (branchID ?name) (likesHardware "si") (explanation ?expl))
    (branch (id ?name) (name ?fullName))

        =>

    (retract ?rule)
    (assert (ansAlpha ?fullName ?expl))
)

;   - Estas dos siguientes reglas comprenden el resto de las hojas del "arbol de decisión" implícito que se utiliza, capturan 
;     las diversas respuestas y acorde a lo que se posee en la base de conocimientos, se da una respuesta.
(defrule reccomender2
    (declare (salience 50))
    (module ALPHA)
    ?rule <- (deduction)

    (answer hardware ?a)
    (answer programar ?b)
    (answer promedio ?c)
    (answer matematicas ?d)
;   Utilizar el $? permite que si encuentra un valor de los que puede tener esa respuesta, se acepta y dispara la regla.
    (recommendationAlpha (branchID ?name) (likesHardware $? ?a $?) (likesToProgram $? ?b $?) (averageGrade $? ?c $?) (likesMath $? ?d $?) (explanation ?expl)) 
    (branch (id ?name) (name ?fullName))
    
        =>
    (retract ?rule)
    (assert (ansAlpha ?fullName ?expl))
)

(defrule reccomender3
    (declare (salience 50))
    (module ALPHA)
    ?rule <- (deduction)

    (answer hardware ?a)
    (answer programar ?b)
    (answer trabajo ?c)
    (answer matematicas ?d)

    (recommendationAlpha (branchID ?name) (likesHardware $? ?a $?) (likesToProgram $? ?b $?) (howMuchWork $? ?c $?)  (likesMath $? ?d $?) (explanation ?expl))    
    (branch (id ?name) (name ?fullName))
    
        =>
    (retract ?rule)
    (assert (ansAlpha ?fullName ?expl))
)

; Esta regla se dispara cuando el razonador no encuentra una recomendacion de rama por falta de información.
(defrule reccomendDefault
    (declare (salience -10))
    (module ALPHA)

    (answer hardware ? )
    (answer programar ? )
    (answer matematicas ?)
    (answer promedio|trabajo ? )

    ?rule <- (deduction)
        =>
    (printout t crlf "### RESPUESTA RECOMENDADOR RAMAS ###" crlf ">>Lo siento, con la informacion que me has dado no puedo llegar a una conclusion satisfactoria." crlf)
    (retract ?rule)
)

; Imprimir recomendación
(defrule giveRecc
    (ansAlpha ?fullName ?expl)
    ?rule <- (module ALPHA)
        =>
    (printout t crlf "### RESPUESTA RECOMENDADOR RAMAS ###" crlf ">>Te recomiendo " ?fullName "." crlf ">>" ?expl "."crlf)
    (retract ?rule)
)
; ##########################
; # FIN ALPHA
; ##########################

; ##########################
; # INICIO GAMMA
; ##########################

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Modulo ejecutar sistema difuso con varias variables de entrada   ;;;;;;;
;;;;;       MODULO CALCULO FUZZY (modulo calculo_fuzzy)      ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  Copyright Juan Luis Castro  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCIONES NECESARIAS  ;;;;;;;;;;;;;;;;;;;;;;;

; Membership es una funcion que calcul el el grado de pertenencia de value al conjunto difuso 
;trapezoidal (a b c d). 
(deffunction membership (?value ?a ?b ?c ?d)
    (if  (< ?value ?a) then (bind ?rv 0)
        else 
        (if (< ?value ?b) then (bind ?rv (/ (- ?value ?a) (- ?b ?a)))
            else
                (if  (< ?value ?c) then (bind ?rv 1)
                    else
                    (if (< ?value ?d) then (bind ?rv (/ (- ?d ?value) (- ?d ?c)))
                            else (bind ?rv 0)
                    )				   
                )			  
        )
    )
    ?rv
)

; center_of_gravity es una función que calcula el centro de gravedad del conjunto difuso
;trapezoidal (a b c d).
(deffunction center_of_gravity (?a ?b ?c ?d)
    (bind ?ati (/ (- ?b ?a) 2))
    (bind ?atd (/ (- ?d ?c) 2))
    (bind ?rv (/ (+ (+ ?b ?c) (- ?atd ?ati)) 2))
    ?rv
)

(deffunction conjuncion (?x ?y)
    (bind ?rv (* ?x ?y))
    ?rv
)

 
;;;;;;;;;;;;;;;;;;;;;;;;;; CONOCIMIENTO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Definimos de las variables del sistema

(deffacts variables_difusas
    (variable work)
    (variable practice)
    (variable theory)
    (variable typeEval)
)

(deffacts conjuntos_difusos
    
    ; Trabajo
    (cd work high 3.5 5 999 999)     ; Aproximadamente 5 o más horas diarias de trabajo.      
    (cd work mid 1.25 2 4 5)         ; ''              entre 3 y 5 horas diarias de trabajo.
    (cd work low 0 0 1 1.75)         ; ''              3 o menos horas diarias de trabajo.

    ; Practica
    (cd practice high 6.5 7.25 999 999)   ; Aproximadamente 7 o más de gusto por prácticas de 10.
    (cd practice mid 2.5 3.25 6.5 7.5)    ; Aproximadamente entre 3 y 7 de gusto por prácticas de 10.
    (cd practice low 0 0 2.5 3.5)         ; Aproximadamente entre 0 y 3 de gusto por prácticas de 10.
    
    ; Teoría
    (cd theory high 6.5 7.25 999 999)     ;Ídem para teoría.
    (cd theory mid 2.5 3.25 6.5 7.5)
    (cd theory low 0 0 2.5 3.5)

    ; Evaluación
    (cd typeEval cont 6.5 7.25 999 999)     ; Aproximadamente 6 o más, más hacia 10 indica que le gusta evaluacion continua.
    (cd typeEval neutral 2.5 3.25 6.5 7.5)  ; Aproximadamente Entre 3 y 6, le da igual que sea evaluacion continua o todo al final.
    (cd typeEval final 0 0 2.5 3.5)         ; Aproximadamente menos de 3, mientra más hacia 0 más le gusta evaluacion al final.
)

(defrule cumplimiento_predicado_difuso
    (declare (salience 3))
    (modulo calculo_fuzzy)
    (cd ?v ?l ?a ?b ?c ?d)
    (dato ?v ?x)
    =>
    (bind ?g (membership ?x ?a ?b ?c ?d))
    (assert (fuzzy cumplimiento ?v ?l ?g))
    ;(if (> ?g 0) then (printout t ">>"?v " es " ?l " en grado " ?g crlf))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Obtenemos el matching de cada antecedente de cada regla
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule matching_antecedente_simple
(declare (salience 2))
(modulo calculo_fuzzy)
(regla ?r antecedente ?v ?l)
(fuzzy cumplimiento ?v ?l ?g)
=>
(assert (fuzzy matching ?r ?g ?v))
)

(defrule matching_antecedente_1
(declare (salience 2))
(modulo calculo_fuzzy)
?f <- (fuzzy matching ?r ?g ?v)
(not (fuzzy matching_antecedente_regla ?r ?))
=>
(assert (fuzzy matching_antecedente_regla ?r ?g))
(retract ?f)
)

(defrule matching_antecedente
(declare (salience 2))
(modulo calculo_fuzzy)
?f <- (fuzzy matching ?r ?g ?v)
?h<- (fuzzy matching_antecedente_regla ?r ?g1)
=>
(retract ?f ?h)
(assert (fuzzy matching_antecedente_regla ?r (conjuncion ?g1 ?g)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inferimos con su correspondiente grado los consecuentes de las reglas que hacen algun matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule inferencia_difusa
(declare (salience 1))
(modulo calculo_fuzzy)
(fuzzy matching_antecedente_regla ?r ?g1)
(test (> ?g1 0))
(regla ?r consecuente ?v ?l)
(regla ?r explicacion ?text)
=>
(assert (fuzzy inferido ?v ?l ?g1))
(printout t ">> " ?text  crlf)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Obtenemos el valor deducido como la media ponderada (por los grados de cada consecuente) 
;;;  de los centros de gravedad de los consecuentes inferidos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule concrecion_individual
(modulo calculo_fuzzy)
(fuzzy inferido ?v ?l ?g1)
(cd ?v ?l ?a ?b ?c ?d)
=>
(assert (fuzzy sumando_numerador ?v (* ?g1 (center_of_gravity ?a ?b ?c ?d))))
(assert (fuzzy sumando_denominador ?v ?g1))
)

(defrule concrecion_numerador
(modulo calculo_fuzzy)
?g<- (fuzzy numerador ?v ?x)
?f <- (fuzzy sumando_numerador ?v ?y)
=>
(assert (fuzzy numerador ?v (+ ?x ?y)))
(retract ?f ?g)
)

(defrule concrecion_denominador
(modulo calculo_fuzzy)
?g<- (fuzzy denominador ?v ?x)
?f <- (fuzzy sumando_denominador ?v ?y)
=>
(assert (fuzzy denominador ?v (+ ?x ?y)))
(retract ?f ?g)
)

(defrule respuesta
    (declare (salience -1))
    (modulo calculo_fuzzy)
    (fuzzy numerador ?v ?n)
    (fuzzy denominador ?v ?d)
    (test (> ?d 0))
    =>
    (assert (fuzzy valor_inferido ?v (/ ?n ?d)))
    ;(printout t ">>Dado estos hechos " ?v " es " (/ ?n ?d)  crlf)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  Una vez inferido el valor salimos del modulo

(defrule salir
    (declare (salience -2))
    ?f <- (modulo calculo_fuzzy)
    =>
    (retract ?f)
)

;;;;;;;;;;;;;;;;;;;; INICIALIZACION DE LOS VALORES CUANDO SE ENTRA AL MODULO  ;;;;;;;;;;

(defrule iniciar_proceso
    (declare (salience 5))
    (modulo calculo_fuzzy)
    =>
    (assert (borrar_datos_ejecucion_anterior))
)

(defrule borrar_datos_ejecucion_anterior
    (declare (salience 5))
    (modulo calculo_fuzzy)
    (borrar_datos_ejecucion_anterior)
    ?f <- (fuzzy $?)
    =>
    (retract ?f)
)

(defrule borrar_borrar_datos
    (declare (salience 5))
    (modulo calculo_fuzzy)
    ?f<- (borrar_datos_ejecucion_anterior)
    (not (fuzzy $?))
    =>
    (retract ?f)
)

(defrule inicializar_fuzzy_inference
    (declare (salience 4))
    (modulo calculo_fuzzy)
    (regla ? consecuente ?v ?)
    =>
    (assert (fuzzy numerador ?v 0))
    (assert (fuzzy denominador ?v 0))
)
; ##########################
; # FIN GAMMA
; ##########################

; ##########################
; # INCIO DELTA
; ##########################

;   HECHOS
;   - Se crea un registro para la base de conocimiento. Se almacenan las asignaturas con información ya recabada de los Expertos y otras fuentes.
(deftemplate course
    (field ID)
    (field name)
    (field theory)
    (field practice)
    (field typeEv)
    (field work)
)

;   - Hecho para verificar (posteriormente) que una asignatura que se ha ingresado existe en la Base de Conocimiento
(deffacts validCourses
    (validCourse FFT FS FP ALEM CA ES TOC IES LMD MP SCD PDOO SO ED EC ALG FIS AC FBD IA IG ISE DDSI MC FR)
)

;   - Se definen las asignaturas con información de como son de teoricas, practicas, su tipo de evaluacion y que tan trabajosa es.
(deffacts courseDataBase

;   1º Curso 1º Cuatrimetre
    (course 
        (ID FFT)
        (name "Fundamentos Fisicos y Tecnologicos")
        (theory low)
        (practice high)
        (typeEv final)
        (work high)
    )   

    (course 
        (ID FS)
        (name "Fundamentos de Software")
        (theory mid)
        (practice mid)
        (typeEv cont)
        (work mid)
    )

    (course 
        (ID FP)
        (name "Fundamentos de Programacion")
        (theory low)
        (practice high)
        (typeEv lovesFinal)
        (work high)
    )   

    (course 
        (ID ALEM)
        (name "Algebra Lineal y Estructuras Matematicas")
        (theory doesntLike)
        (practice loves)
        (typeEv lovesFinal)
        (work high)
    )   

    (course 
        (ID CA)
        (name "Calculo")
        (theory doesntLike)
        (practice loves)
        (typeEv lovesFinal)
        (work high)
    )   

;   1º Curso 2º Cuatrimetre
    (course 
        (ID ES)
        (name "Estadistica")
        (theory high)
        (practice low)
        (typeEv final)
        (work mid)
    )

    (course 
        (ID TOC)
        (name "Tecnologia y Organizacion de Computadores")
        (theory doesntLike)
        (practice loves)
        (typeEv lovesFinal)
        (work high)
    )   

    (course 
        (ID IES)
        (name "Ingenieria, Empresa y Sociedad")
        (theory high)
        (practice low)
        (typeEv neutral)
        (work low)
    )

    (course 
        (ID LMD)
        (name "Logica y Metodos Discretos")
        (theory doesntLike)
        (practice loves)
        (typeEv lovesFinal)
        (work high)
    )

    (course 
        (ID MP)
        (name "Metodologia de Programacion")
        (theory doesntLike)
        (practice loves)
        (typeEv lovesFinal)
        (work high)
    )

;   2º Curso 1º Cuatrimestre
    (course 
        (ID SCD)
        (name "Sistemas Concurrentes y Distribuidos")
        (theory high)
        (practice high)
        (typeEv neutral)
        (work high)
    )        

    (course 
        (ID PDOO)
        (name "Programacion y Disenio Orientado a Objetos")
        (theory low)
        (practice high)
        (typeEv neutral)
        (work high)
    )

    (course 
        (ID SO)
        (name "Sistemas Operativos")
        (theory mid)
        (practice mid)
        (typeEv neutral)
        (work mid)
    )          

    (course 
        (ID ED)
        (name "Estructuras de Datos")
        (theory doesntLike)
        (practice loves)
        (typeEv lovesFinal)
        (work high)
    )   

    (course 
        (ID EC)
        (name "Estructura de Computadores")
        (theory doesntLike)
        (practice loves)
        (typeEv lovesFinal)
        (work high)
    )   

;   2º Curso 2º Cuatrimestre
    (course 
        (ID ALG)
        (name "Algoritmica")
        (theory high)
        (practice mid)
        (typeEv final)
        (work high)
    )   

    (course 
        (ID FIS)
        (name "Fundamentos de Ingenieria del Software")
        (theory high)
        (practice mid)
        (typeEv cont)
        (work mid)
    )   

    (course 
        (ID AC)
        (name "Arquitectura de Computadores")
        (theory high)
        (practice high)
        (typeEv neutral)
        (work high)
    )   

    (course 
        (ID FBD)
        (name "Fundamentos de Base de Datos")
        (theory mid)
        (practice high)
        (typeEv cont)
        (work mid)
    )

    (course 
        (ID IA)
        (name "Inteligencia Artificial")
        (theory low)
        (practice high)
        (typeEv final)
        (work high)
    )      

;   3º Curso 1º Cuatrimestre
    (course 
        (ID IG)
        (name "Informatica Grafica")
        (theory low)
        (practice high)
        (typeEv final)
        (work high)
    )   

    (course 
        (ID ISE)
        (name "Ingenieria de Servidores")
        (theory high)
        (practice mid)
        (typeEv final)
        (work mid)
    )   

    (course 
        (ID DDSI)
        (name "Disenio y Desarrollo de Sistemas de Informacion")
        (theory low)
        (practice high)
        (typeEv cont)
        (work low)
    )   

    (course 
        (ID MC)
        (name "Modelos de Computacion")
        (theory high)
        (practice low)
        (typeEv cont)
        (work low)
    )   

    (course 
        (ID FR)
        (name "Fundamentos de Redes")
        (theory high)
        (practice low)
        (typeEv final)
        (work mid)
    )   
)


;   Definición de preguntas a realizar.
(deffacts questionsDelta
    (question work)
    (question practice)
    (question theory)
    (question typeEval)
    (question priority)
    (question firstCourse)
    (question secondCourse)
)

;   REGLAS
;   Preguntas:
;   - Regla inicial.
(defrule startUpDelta
    (declare (salience 100))
    (module DELTA)
        =>
    (printout t "-- ELEGIR ENTRE DOS ASIGNATURAS PARA MATRICULAR --" crlf )
    (printout t ">>Indica dos asignaturas entre 1er Curso y 3er Curso 1er Cuatrimestre del Grado de Ingenieria Informatica en la UGR" crlf crlf)
    ; Se usa este hecho para evitar que se dispare una sola recomendacion.
    (assert (deduction))
)

;   - Se preguntan por los cursos.
(defrule askCourse1
    (deduction)
    (module DELTA)
    ?r <- (question firstCourse)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf ">>Escribir la primera asignatura" crlf ">")
    (bind ?answer (read))
    (retract ?r)
    (assert (delta course ?answer firstCourse))
)

(defrule askCourse2
    (deduction)
    (module DELTA)
    ?r <- (question secondCourse)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf ">>Escribir la segunda asignatura" crlf ">")
    (bind ?answer (read))
    (retract ?r)

    (assert (delta course ?answer secondCourse))
)

;   - Se chequea si los cursos están en la Base de Conocimiento.
(defrule checkCourseExists
    (deduction)
    (module DELTA)
    ?r <- (delta course ?ans ?question)
    ; Si no se encuentra en la regla validCourse, no está en la BC.
    (not (validCourse $? ?ans $?))
        =>
    (printout t ">>Perdona pero '"?ans"' no es un curso valido de entre 1er Curso y 3er Curso 1er Cuatrimestre de GII. Vuelve a intentar." crlf)
    (retract ?r)
    (assert (question ?question))
)
;   - Se hacen preguntas relativas a las caracteristicas del alumno.
(defrule askWork
    (deduction)
    (module DELTA)
    ?r <- (question work)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf  ">>En promedio, cuantas horas le dedicas al estudio diariamente? " crlf ">>(Numero fraccional mayor o igual a 0)" crlf">")
    (bind ?answer (read))
    (retract ?r)

    (if(<= 0 ?answer) then
        (assert (dato work ?answer))
    else
        (printout t ">>Error: Respuesta invalida, vuelva a intentar.")
        (assert (question work))
    )
)

(defrule askPractice
    (deduction)
    (module DELTA)
    ?r <- (question practice)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf  ">>Entre 0 y 10, que tanto te gusta realizar practicas en una asignatura?" crlf ">>(Numero fraccional entre [0 - Lo odio | 10 - Me encanta])" crlf">")
    (bind ?answer (read))
    (retract ?r)

    (if (and (<= 0 ?answer)  (<= ?answer 10)) then
        (assert (dato practice ?answer))
    else
        (printout t ">>Error: Respuesta invalida, vuelva a intentar.")
        (assert (question practice))
    )
)

(defrule askTheory
    (deduction)
    (module DELTA)
    ?r <- (question theory)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf  ">>Entre 0 y 10, que tanto te gusta la teoria en una asignatura?" crlf ">>(Numero fraccional entre [0 - Lo odio | 10 - Me encanta])" crlf">")
    (bind ?answer (read))
    (retract ?r)

    (if (and (<= 0 ?answer)  (<= ?answer 10)) then
        (assert (dato theory ?answer))
    else
        (printout t ">>Error: Respuesta invalida, vuelva a intentar.")
        (assert (question theory))
    )
)

(defrule asktypeEval
    (deduction)
    (module DELTA)
    ?r <- (question typeEval)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf  ">>Entre 0 y 10, te gusta mas evaluacion continua o evaluacion al final de todo?" crlf ">>(Numero fraccional entre [0 - Me gusta mas evaluacion final  | 10 - Me gusta mas evaluacion continua])" crlf">")
    (bind ?answer (read))
    (retract ?r)

    (if (and (<= 0 ?answer)  (<= ?answer 10)) then
        (assert (dato typeEval ?answer))
        (assert (modulo calculo_fuzzy))
        (printout t crlf crlf "--- RAZONAMIENTO ---" crlf)
    else
        (printout t ">>Error: Respuesta invalida, vuelva a intentar.")
        (assert (question typeEval))
    )
)

;   Deducciones
;   - Se generan, para las asignaturas que se han seleccionado, las reglas de antecentes para la lógica difusa.
(defrule makeCDforCourse
    (delta course ?a ?)
        =>
    (assert (cd ?a high 7 7.5 11 11)) ; Si es alto, es decir, entre aprox 7 y 10, es una recomendación alta para esa asignatura.
    (assert (cd ?a mid  4.75 5.5 6.5 7.25))
    (assert (cd ?a low  0 0 3.5 5))
)

;   - Se generan dinámicamente las reglas paras las asignaturas elegidas.
;       + Reglas de Teoría
(defrule genRuleTheory
    (delta course ?a ?)
    (course (ID ?a) (theory ?b))
        =>
    ; Si la asignatura es muy teórica, entonces, ajustar las reglas que se pueden disparar acordemente y sus razonamientos.
    (if (eq high ?b) then

        ; "Si al alumno le gusta la teoria y la asignatura es teorica, recomendacion alta."
        (bind ?y (str-cat "high-theory" ?a "-plus"))
        (bind ?x (str-cat  ?a " es teorica y te gusta teoria: es un punto a favor."))
        (assert (regla ?y antecedente theory high))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        ; "Si al alumno le da igual la teoria y la asignatura es teorica, mas o menos recomendada."
        (bind ?y (str-cat "high-theory" ?a "-medium"))
        (bind ?x (str-cat ?a " es teorica y te gusta mas o menos teoria: es un punto neutro."))
        (assert (regla ?y antecedente theory mid))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        ; "Si al alumno no le gusta la teoría y la asignatura es teorica, no es recomendable."
        (bind ?y (str-cat "high-theory" ?a "-low"))
        (bind ?x (str-cat ?a " es teorica y no te gusta teoria: es un punto en contra."))
        (assert (regla ?y antecedente theory low))
        (assert (regla ?y consecuente ?a low))
        (assert (regla ?y explicacion ?x))

    ;   Ídem como antes.
    else(if (eq mid ?b) then

        (bind ?y (str-cat "mid-theory" ?a "-plus"))
        (bind ?x (str-cat ?a " es mas o menos teorica y te gusta teoria: es un punto neutro."))
        (assert (regla ?y antecedente theory high))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "mid-theory" ?a "-medium"))
        (bind ?x (str-cat ?a " es mas o menos teorica y asi te gusta: es un punto a favor."))
        (assert (regla ?y antecedente theory mid))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "mid-theory" ?a "-low"))
        (bind ?x (str-cat ?a " es mas o menos teorica y no te gusta teoria: es un punto neutro."))
        (assert (regla ?y antecedente theory low))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))
    else

        (bind ?y (str-cat "low-theory" ?a "-plus"))
        (bind ?x (str-cat ?a " no es teorica y te gusta teoria: es un punto en contra."))
        (assert (regla ?y antecedente theory high))
        (assert (regla ?y consecuente ?a low))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "low-theory" ?a "-medium"))
        (bind ?x (str-cat ?a " no es teorica y te gusta mas o menos teoria: es un punto neutro."))
        (assert (regla ?y antecedente theory mid))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "low-theory" ?a "-low"))
        (bind ?x (str-cat ?a " no es teorica y no te gusta teoria: es un punto a favor."))
        (assert (regla ?y antecedente theory low))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))
    ))
)

;       + Reglas de Prácticas
(defrule genRulePractice
    (delta course ?a ?)
    (course (ID ?a) (practice ?b))
        =>
    (if (eq high ?b) then

        (bind ?y (str-cat "high-practice" ?a "-plus"))
        (bind ?x (str-cat  ?a " es practica y te gusta practica: es un punto a favor."))
        (assert (regla ?y antecedente practice high))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "high-practice" ?a "-medium"))
        (bind ?x (str-cat  ?a " es practica y te gusta mas o menos practica: es un punto neutro."))
        (assert (regla ?y antecedente practice mid))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "high-practice" ?a "-low"))
        (bind ?x (str-cat ?a " es practica y no te gusta practica: es un punto en contra."))
        (assert (regla ?y antecedente practice low))
        (assert (regla ?y consecuente ?a low))
        (assert (regla ?y explicacion ?x))

    else(if (eq mid ?b) then

        (bind ?y (str-cat "mid-practice" ?a "-plus"))
        (bind ?x (str-cat  ?a " es mas o menos practica y te gusta practica: es un punto neutro."))
        (assert (regla ?y antecedente practice high))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "mid-practice" ?a "-medium"))
        (bind ?x (str-cat  ?a " es mas o menos practica y te gusta practica: es un punto a favor."))
        (assert (regla ?y antecedente practice mid))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "mid-practice" ?a "-low"))
        (bind ?x (str-cat  ?a " es mas o menos practica y no te gusta practica: es un punto neutro."))
        (assert (regla ?y antecedente practice low))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))
    else
        (bind ?y (str-cat "low-practice" ?a "-plus"))
        (bind ?x (str-cat  ?a " no es practica y te gusta practica: es un punto en contra."))
        (assert (regla ?y antecedente practice high))
        (assert (regla ?y consecuente ?a low))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "low-practice" ?a "-medium"))
        (bind ?x (str-cat  ?a " no es practica y te gusta mas o menos practica: es un punto neutro."))
        (assert (regla ?y antecedente practice mid))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "low-practice" ?a "-low"))
        (bind ?x (str-cat  ?a " no es practica y no te gusta practica: es un punto a favor."))
        (assert (regla ?y antecedente practice low))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))
    ))
)

;       + Reglas de que trabajoso es.
(defrule genRuleWork
    (delta course ?a ?)
    (course (ID ?a)  (work ?b))
        =>
    (if (eq high ?b) then
        (bind ?y (str-cat "high-work" ?a "-plus"))
        (bind ?x (str-cat  ?a " es trabajosa y eres aplicado: es un punto a favor"))
        (assert (regla ?y antecedente work high))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x ))

        (bind ?y (str-cat "high-work" ?a "-medium"))
        (bind ?x (str-cat  ?a " es trabajosa y eres medio aplicado: es un punto neutro"))
        (assert (regla ?y antecedente work mid))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x ))

        (bind ?y (str-cat "high-work" ?a "-low"))
        (bind ?x (str-cat  ?a " es trabajosa y no eres aplicado: es un punto en contra"))
        (assert (regla ?y antecedente work low))
        (assert (regla ?y consecuente ?a low))
        (assert (regla ?y explicacion ?x ))

    else(if (eq mid ?b) then
        (bind ?y (str-cat "mid-work" ?a "-plus"))
        (bind ?x (str-cat  ?a " es  mas o menos trabajosa y eres aplicado: es un punto a favor, mas tiempo libre."))
        (assert (regla ?y antecedente work high))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "mid-work" ?a "-medium"))
        (bind ?x (str-cat  ?a " es mas o menos trabajosa y eres medio aplicado: es un punto a favor."))
        (assert (regla ?y antecedente work mid))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "mid-work" ?a "-low"))
        (bind ?x (str-cat  ?a " es  mas o menos trabajosa y no eres aplicado: es un punto en contra."))
        (assert (regla ?y antecedente work low))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))
    else
        (bind ?y (str-cat "low-work" ?a "-plus"))
        (bind ?x (str-cat  ?a " no es trabajosa y eres aplicado: es un punto a favor, se aprueba sola."))
        (assert (regla ?y antecedente work high))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "low-work" ?a "-medium"))
        (bind ?x (str-cat  ?a " no es trabajosa y eres medio aplicado: es un punto a favor, relax."))
        (assert (regla ?y antecedente work mid))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "low-work" ?a "-low"))
        (bind ?x (str-cat  ?a " no es trabajosa y eres no eres aplicado: es un punto a favor."))
        (assert (regla ?y antecedente work low))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))
    ))
)

        ; + Reglas del tipo de evaluación
(defrule genRuleEval
    (delta course ?a ?)
    (course (ID ?a) (typeEv ?b))
        =>
    (if (eq cont ?b) then
        (bind ?y (str-cat "cont-typeEval" ?a "-plus"))
        (bind ?x (str-cat  ?a " utiliza mayoritariamente evaluacion continua y te gusta: es un punto a favor."))
        (assert (regla ?y antecedente typeEval cont))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "cont-typeEval" ?a "-medium"))
        (bind ?x (str-cat  ?a " utiliza mayoritariamente evaluacion continua y da igual: es un neutro."))
        (assert (regla ?y antecedente typeEval neutral))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "cont-typeEval" ?a "-low"))
        (bind ?x (str-cat  ?a " utiliza mayoritariamente evaluacion continua y no te gusta asi: es un punto en contra."))
        (assert (regla ?y antecedente typeEval final))
        (assert (regla ?y consecuente ?a low))
        (assert (regla ?y explicacion ?x))

    else(if (eq neutral ?b) then
        (bind ?y (str-cat "mid-typeEval" ?a "-plus"))
        (bind ?x (str-cat  ?a " tiene un balance de evaluacion continua y al final pero prefieres continua: es un punto neutro."))
        (assert (regla ?y antecedente typeEval cont))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "mid-typeEval" ?a "-medium"))
        (bind ?x (str-cat  ?a " tiene un balance de evaluacion continua y al final y te da igual: es un punto a favor."))
        (assert (regla ?y antecedente typeEval neutral))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "mid-typeEval" ?a "-low"))
        (bind ?x (str-cat  ?a " tiene un balance de evaluacion continua y al final pero prefieres al final: es un punto neutro."))
        (assert (regla ?y antecedente typeEval final))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))
    else
        (bind ?y (str-cat "final-typeEval" ?a "-plus"))
        (bind ?x (str-cat  ?a " utiliza mayoritariamente evaluacion al final y no te gusta asi: es un punto en contra"))
        (assert (regla ?y antecedente typeEval cont))
        (assert (regla ?y consecuente ?a low))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "final-typeEval" ?a "-medium"))
        (bind ?x (str-cat  ?a " utiliza mayoritariamente evaluacion al final y te da igual: es un punto neutro"))
        (assert (regla ?y antecedente typeEval neutral))
        (assert (regla ?y consecuente ?a mid))
        (assert (regla ?y explicacion ?x))

        (bind ?y (str-cat "final-typeEval" ?a "-low"))
        (bind ?x (str-cat  ?a " utiliza mayoritariamente evaluacion al final y asi te gusta: es un punto a favor"))
        (assert (regla ?y antecedente typeEval final))
        (assert (regla ?y consecuente ?a high))
        (assert (regla ?y explicacion ?x))
    ))
)

;   - Se define un registro sencillo para convertir lo que retorna el módulo GAMMA para imprimir por pantalla la recomendación.
(deftemplate endValue
    (slot ID)
    (slot val)
)

;   - Regla que realiza la conversión.
(defrule fillEndValue
    (module DELTA)
    (fuzzy valor_inferido ?a ?b)
        =>
    (assert (endValue (ID ?a) (val ?b)))
)

;   - Regla que imprime por pantalla la recomendación dados los razonamientos del módulo GAMMA.
(defrule giveAnswer
    (declare (salience -10))
    ?r <- (module DELTA)
    (endValue (ID ?name1) (val ?value1))
    ; Esto es para que, de las dos asignaturas, se imprima el valor de recomendación mayor.
    (not (endValue (val ?value2&:(> ?value2 ?value1))))
    (course (ID ?name1) (name ?fullName))
        =>
    (printout t crlf "### RESPUESTA RECOMENDADOR MATRICULA ###" crlf ">> Dados los hechos expuestos anteriormente, recomiendo " ?fullName "." crlf ">> Le doy una valoracion de " ?value1"/10 a que te va a gustar." crlf)
    (retract ?r)
)

; ##########################
; # FIN DELTA
; ##########################

; ##########################
; # INICIO MAIN 
; ##########################

(deffacts startFact
    (start)
)

(defrule startUp
    ?fact <- (start)
        =>
    (printout t crlf crlf"--- Sistema Basado en Conocimiento ---" crlf)
    (printout t ">>Que funcionalidad deseas elegir?" crlf)
    (printout t ">>> 1 - Ayuda para elegir una mencion de la carrera." crlf)
    (printout t ">>> 2 - Ayuda para elegir entre dos asignaturas." crlf ">")

    (bind ?answer (read))
    (retract ?fact)

    (if (= ?answer 1) then
        (assert (module ALPHA))
    else (if (= ?answer 2) then 
        (assert (module DELTA))
    else
        (printout t ">>Error: Respuesta invalida, vuelva a intentar.")
        (assert (start))
    ))
)

; ##########################
; # FIN MAIN
; ##########################