; [CASTELLANO]
; Practica 4.1: Sistema Experto Simple con Modulos.
; Asignatura: Ingenieria del Conocimiento
; Autor: Antonio Galera Gazques y Valentino Lugli (Github: @RhinoBlindado)
; Fecha: Mayo, Junio 2021

; [ENGLISH]
; Practice 4.1: Simple Expert System with Modules.
; Course: Knowledge Engineering
; Author: Antonio Galera Gazques and Valentino Lugli (Github: @RhinoBlindado)
; Date: May, June 2021


; ##########################
; # INICIO MAIN
; ##########################

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
    (question profesion)
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

    (validAnswers (questionName profesion)
        (validAns "docencia" "publica" "privada" "me da igual")
    )      
)

; REGLAS
; Regla inicial.
(defrule startUp
    (declare (salience 100))
        =>
    (printout t crlf crlf"--- Sistema Experto Simple con Modulos ---" crlf)
    (printout t "-- RECOMENDACIONES SOBRE RAMAS --" crlf crlf)
    ; Se usa este hecho para evitar que se dispare una sola recomendacion.
    (assert (deduction))
)


; Reglas que obtienen informacion del usuario.
(defrule hardwareQuestion
    (deduction)
    ?rule <- (question hardware)
        =>
    (retract ?rule)
    (printout t crlf "--- PREGUNTA ---" crlf ">>Te gusta el hardware?" crlf ">>(Si | No | Me da igual)" crlf ">")
    ; Leer la entrada con espacios y convertirla a minuscula.
    (assert(answer hardware (lowcase (readline))))
)

(defrule professionQuestion
    (deduction)
    ?rule <- (question profesion)
        =>
    (retract ?rule)
    (printout t crlf "--- PREGUNTA ---" crlf ">>Donde te gustaria trabajar en un futuro?" crlf ">>(Docencia | Publica | Privada | Me da igual)" crlf ">")
    (assert (answer profesion (lowcase(readline))))
)

(defrule programmingQuestion
    (deduction)
    ?rule <- (question programar)
        =>
    (retract ?rule)
    (printout t crlf "--- PREGUNTA ---" crlf ">>Te gusta programar?" crlf ">>(Si | No | Me da igual)" crlf ">")
    (assert(answer programar (lowcase (readline))))
)

(defrule mathQuestion
    (deduction)
    ?rule <- (question matematicas)
        =>
    (retract ?rule)
    (printout t crlf "--- PREGUNTA ---" crlf ">>Te gustan las matematicas?" crlf ">>(Si | No | Me da igual)" crlf ">")
    (assert(answer matematicas (lowcase(readline))))
)

(defrule workQuestion
    (deduction)
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
    (answer matematicas "si"|"me da igual")
    ?rule <- (question promedio)
        =>
    (printout t crlf "--- PREGUNTA ---" crlf ">>Cual es tu promedio?" crlf ">>(Numero fraccional entre 5 y 10)" crlf ">")
    ; Primero se crea este hecho para convertir de número a un valor discreto.
    (assert(answerNum promedio (read)))
    (retract ?rule)
    (assert (module ALPHA))
)

;   Convertir promedio a valor discreto.
(defrule gradeQuestionConvertAnswer
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

;   Reglas de verificación de correctitud de las respuestas.
;   - Verificar que en las preguntas textuales se responde acordemente.
(defrule checkAns
    (declare (salience 90))

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
    ?rule <- (answerNum ?type ?ans)
    ; Si el numero es menos de 5 o mayor que 10 o diferente de 0...
    (test ( or (< ?ans 5) (> ?ans 10) ) )        
        =>
    ; ... Imprimir el mensaje y retornar a la pregunta.
    (printout t ">>Perdona, pero '"?ans "' no es una respuesta valida sobre " ?type ". Vuelve a responder." crlf)
    (retract ?rule)
    (assert (question ?type))
)

; ##########################
; # FIN MAIN
; ##########################

; ##########################
; # INICIO MODULO ALPHA
; # Autor: Valentino Lugli
; ##########################

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

;   Reglas de selección de rama
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

;   Esta regla se dispara cuando el razonador no encuentra una recomendacion de rama por falta de información.
(defrule reccomendDefault
    (declare (salience -10))
    (module ALPHA)

    (answer hardware ? )
    (answer programar ? )
    (answer matematicas ?)
    (answer promedio|trabajo ? )

    ?rule <- (deduction)
        =>
    (printout t crlf "### RESPUESTA MODULO ALPHA ###" crlf ">>Lo siento, con la informacion que me has dado no puedo llegar a una conclusion satisfactoria." crlf)
    (retract ?rule)
    (assert (module BETA))
)

;   Imprimir recomendación
(defrule giveRecc
    (ansAlpha ?fullName ?expl)
    ?rule <- (module ALPHA)
        =>
    (printout t crlf "### RESPUESTA MODULO ALPHA ###" crlf ">>Te recomiendo " ?fullName "." crlf ">>" ?expl "."crlf)
    (retract ?rule)
    (assert (module BETA))
)

; ##########################
; # FIN ALPHA
; ##########################

; ##########################
; # MODULO BETA
; # Autor: Antonio Galera Gazques
; ##########################

(deffacts Ramas
    (Rama Computacion_y_Sistemas_Inteligentes)
    (Rama Ingenieria_del_Software)
    (Rama Ingenieria_de_Computadores)
    (Rama Sistemas_de_Informacion)
    (Rama Tecnologias_de_la_Informacion)
)

(defrule consejoIC
    (module BETA)
    (answer hardware "si")
=>
    (assert (Consejo Ingenieria_de_Computadores "te gusta el hardware" "Experto seguro"))
)

(defrule consejoTI
    (module BETA)
    (answer hardware "no")
    (answer programar "no")
=>
    (assert (Consejo Tecnologias_de_la_Informacion "no te gusta el hardware ni programar" "Experto seguro"))
)

(defrule consejoCSI
    (module BETA)   
    (answer hardware "no")
    (answer programar "si")
    (answer matematicas "si")
    (or (answer promedio "alto") (answer promedio "medio"))
=>
    (assert (Consejo Computacion_y_Sistemas_Inteligentes "te gusta programar y las mates, y tus notas son altas" "Experto seguro"))
)

(defrule consejoTI2
    (module BETA)
    (answer hardware "no")
    (answer programar "si")
    (answer matematicas "si")
    (answer promedio "bajo")
=>
    (assert (Consejo Tecnologias_de_la_Informacion "te gusta programar y las mates, pero tus notas son algo bajas, te recomiendo TI porque creo que CSI puede serte complicado" "Experto seguro"))
)

(defrule consejoSI
    (module BETA)
    (answer hardware "no")
    (answer programar "si")
    (answer matematicas "no")
    (answer profesion "docencia")
=>
    (assert (Consejo Sistemas_de_Informacion "te gusta programar pero no las mates o el hardware, si en un futuro quieres ser docente te recomiendo SI" "Experto seguro"))
)

(defrule consejoIS
    (module BETA)
    (answer hardware "no")
    (answer programar "si")
    (answer matematicas "no")
    (answer profesion ~"docencia")
=>
    (assert (Consejo Ingenieria_del_Software "te gusta programar pero no las mates, y no quieres ser docente por lo tanto te recomendaria IS" "Experto seguro"))
)

;;;Cuando hay NS no sabia que hacer asi que lo que hago es que el programa te pueda dar mas de una recomendacion por si cambia ese NS en un futuro
(defrule consejoNSHardware
    (module BETA)
    (answer hardware "me da igual")
    (answer programar "no")
=>
    (assert (Consejo Tecnologias_de_la_Informacion "si termina no gustandote hardware(TOC, AC, EC, ISE) no te recomendaria IC" "Experto inseguro con dos opciones"))
    (assert (Consejo Ingenieria_de_Computadores "si al contrario termina gustandote te recomendaria IC" "Experto inseguro con dos opciones"))
)

(defrule consejoNSHardware2
    (module BETA)
    (answer hardware "me da igual")
    (answer programar ~"no")
    (answer matematicas "si")
=>
    (assert (Consejo Computacion_y_Sistemas_Inteligentes "si termina no gustandote hardware(TOC, AC, EC, ISE) pero tienes buenas notas" "Experto inseguro con tres opciones"))
    (assert (Consejo Tecnologias_de_la_Informacion "si termina no gustandote hardware(TOC, AC, EC, ISE) pero no tienes buenas notas deberias evitar CSI" "Experto inseguro con tres opciones"))
    (assert (Consejo Ingenieria_de_Computadores "si al contrario termina gustandote te recomendaria IC" "Experto inseguro con tres opciones"))
)

(defrule consejoNSHardware3
    (module BETA)
    (answer hardware "me da igual")
    (answer programar ~"no")
    (answer matematicas "no")
=>
    (assert (Consejo Sistemas_de_Informacion "si termina no gustandote hardware(TOC, AC, EC, ISE) pero quieres ser docente" "Experto inseguro con tres opciones"))
    (assert (Consejo Ingenieria_del_Software "si termina no gustandote hardware(TOC, AC, EC, ISE) pero no quieres ser docente" "Experto inseguro con tres opciones"))
    (assert (Consejo Ingenieria_de_Computadores "si al contrario termina gustandote te recomendaria IC" "Experto inseguro con tres opciones"))
)

(defrule consejoNSMates
    (module BETA)
    (answer matematicas "me da igual")
=>
    (assert (Consejo Computacion_y_Sistemas_Inteligentes "si terminan gustandote las mates y tienes buenas notas te sera sencillo" "Experto inseguro con cuatro opciones"))
    (assert (Consejo Tecnologias_de_la_Informacion "si terminan gustandote las mates y pero no tienes las mejores notas te sera  mas sencillo que CSI" "Experto inseguro con cuatro opciones"))
    (assert (Consejo Sistemas_de_Informacion "si no terminan gustandote las mates y quieres dedicarte a la docencia" "Experto inseguro con cuatro opciones"))
    (assert (Consejo Ingenieria_del_Software "si no terminan gustandote las mates y no quieres ser docente" "Experto inseguro con cuatro opciones"))

)

;;;Para los consejos usamos el hecho que se nos recomienda en el guion
(defrule showConsejo
    ?rule <- (module BETA)
    (Rama ?r)
    (Consejo ?r ?motivo ?apodo)
=>
    (printout t crlf "### RESPUESTA MODULO BETA ###" crlf)
    (printout t ?apodo " te recomienda la mencion de " ?r ". Porque " ?motivo crlf)
    (retract ?rule)
)

;;;Es imposible saber algunas ramificaciones, si los datos que introduces no corresponden con las reglas que he creado mostraré este mensaje.
(defrule showConsejoNulo
    ?rule <- (module BETA)
    (not(Consejo ?r ?motivo ?apodo))

=>
    (printout t crlf "### RESPUESTA MODULO BETA ###" crlf)
    (printout t "No te puedo recomendar ninguna rama con las respuestas que me has dado." crlf)
    (retract ?rule)
)

; ##########################
; # FIN BETA
; ##########################
