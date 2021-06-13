;;;UNIDAD 3 ENTREGA 1 IC
;;;ANTONIO GALERA GaZQUEZ
;;;SI NO HAY NINGUNA TILDE ES PORQUE CLIPS DA ERRORES CON ELLAS
;;;Mis recomendaciones se basan en la actividad realizada en clase de arboles de decision
;;;como datos tome los aportados por el profesor y algunos cuantos que añadi yo que creo que se ajustan a la realidad.
;;;Aparte duplique alguna vez los datos para tener una muestra mas grande. Una copia del arbol, realizada con weka, va adjunta.
;;;Dos variables la de si se considera trabajador y que parte de una asignatura le gusta mas Weka las interpreto como que no eran importantes
;;;a la hora de tomar una decision con lo que no las añado.
;;;Las caracteristicas que tome y los valores que pueden tomar son:

;;; ¿Le gustan las matematicas? {si,no,NS}
;;; ¿Donde le gustaria trabajar en un futuro? {docencia,publica,privada,NS}
;;; ¿Cual es su media de expediente? {alta,media,baja,NS}
;;; ¿Le gustan las asignaturas de hardware? {si,no,NS}
;;; ¿Le gusta programar? {si,no,NS}

;;;Definimos las ramas

(deffacts Ramas
    (Rama Computacion_y_Sistemas_Inteligentes)
    (Rama Ingenieria_del_Software)
    (Rama Ingenieria_de_Computadores)
    (Rama Sistemas_de_Informacion)
    (Rama Tecnologias_de_la_Informacion)
)

(defrule arranqueSistema
    (declare (salience 100))
=>
    ;;;Las descripciones provienen de la propia pagina del grado, donde se realizan las presentaciones de las menciones
    (printout t "Hola alumno indeciso soy el sistema de recomendacion de ramas, necesito que responda a una serie de preguntas, las ramas disponibles en la UGR son:" crlf)
    (printout t "*CSI(Computacion y Sistemas Inteligentes) enfocada en el campo de la Inteligencia Artificial y los sistemas inteligentes." crlf)
    (printout t "IC(Ingenieria de Computadores) trata de la concepcion, diseño, construccion y mantenimiento de computadores y sistemas informaticos." crlf)
    (printout t "IS(Ingeniera del Software) donde gestionaremos proyectos, analizaremos y programaremos sistemas software, y realizaremos informes y consultorias." crlf)
    (printout t "SI(Sistemas Inteligentes) consisten en un conjunto de elementos orientados al tratamiento y administracion de datos e informacion, organizados y listos para su uso posterior." crlf)
    (printout t "TI(Tecnologias de la Informacion) enfocada a las TIC, gestion de sistemas, redes y seguridad en una organizacion." crlf)
    (printout t "Una vez presentadas las menciones, comenzaremos las preguntas, si en algun momento no esta seguro responda NS, aunque cuanto mas preguntas responda mas fiable sera la recomendacion" crlf crlf crlf)
)



;;;;;;;;Tendremos una regla que haga la pregunta y otra que se asegure de que la respuesta es valida, por ejemplo en mates que sea si,no o NS.

(defrule matesPregunta
    (declare (salience 50))
=>
    (printout t "Le gustan las matematicas {Si,No,NS}?" crlf)
    (assert (mates (read)))
)

(defrule matesValidez
    (declare (salience 75))
    ?respuesta <- (mates ?valor)
    (test (and (neq ?valor Si) (neq ?valor No) (neq ?valor NS)))
=>
    (printout t "Introduzca algo correcto, le repito la pregunta: Le gustan las matematicas {Si,No,NS}?" crlf)
    (retract ?respuesta)
    (assert (mates (read)))
)

(defrule trabajoPregunta
    (declare (salience 50))
=>
    (printout t "Donde le gustaria trabajar en un futuro {Docencia, Publica, Privada, NS}?" crlf)
    (assert (trabajo (read)))
)

(defrule trabajoValidez
    (declare (salience 75))
    ?respuesta <- (trabajo ?valor)
    (test (and (neq ?valor Docencia) (neq ?valor Publica) (neq ?valor Privada) (neq ?valor NS)))
=>
    (printout t "Introduzca algo correcto, le repito la pregunta: Donde le gustaria trabajar en un futuro {Docencia, Publica, Privada, NS}?" crlf)
    (retract ?respuesta)
    (assert (trabajo (read)))
)

(defrule mediaPregunta
    (declare (salience 50))
=>
    (printout t "Cual es su media de expediente? [0,10]?" crlf)
    (printout t "Debe ingresar un numero ya sea entero o decimal" crlf)
    (assert (expediente (read)))
)

(defrule mediaValidez
    (declare (salience 75))
    ?respuesta <- (expediente ?valor)
    (test(and(>= ?valor 0) (<= ?valor 10) (not (numberp ?valor))))
=>
    (printout t "Introduzca algo correcto, le repito la pregunta: Cual es su media de expediente? [0,10]?" crlf)
    (retract ?respuesta)
    (assert (expediente (read)))
)
;;;;Conversores de nota cogen el valor y lo clasifican en Alta, Media, Baja.
(defrule conversorNotaAlta
    (declare (salience 50))
    (expediente ?valor)
    (test (> ?valor 8.5))
=>
    (assert (nota Alta))
)

(defrule conversorNotaMedia
    (declare (salience 50))
    (expediente ?valor)
    (test (and(> ?valor 6.5) (<= ?valor 8.5)))
=>
    (assert (nota Media))
)

(defrule conversorNotaBaja
    (declare (salience 50))
    (expediente ?valor)
    (test (<= ?valor 6.5))
=>
    (assert (nota Baja))
)

(defrule hardwarePregunta
    (declare (salience 50))
=>
    (printout t "Le gustan las asignaturas de hardware {Si,No,NS}?" crlf)
    (assert (hardware (read)))  
)

(defrule hardwareValidez
    (declare (salience 75))
    ?respuesta <- (hardware ?valor)
    (test (and (neq ?valor Si) (neq ?valor No) (neq ?valor NS)))
=>
    (printout t "Introduzca algo correcto, le repito la pregunta: Le gustan las asignaturas de hardware {Si,No,NS}?" crlf)
    (retract ?respuesta)
    (assert (hardware (read)))
)

(defrule programarPregunta
    (declare (salience 50))
=>
    (printout t "Le gusta programar {Si,No,NS}?" crlf)
    (assert (programar (read)))  
)

(defrule programarValidez
    (declare (salience 75))
    ?respuesta <- (programar ?valor)
    (test (and (neq ?valor Si) (neq ?valor No) (neq ?valor NS)))
=>
    (printout t "Introduzca algo correcto, le repito la pregunta: Le gusta programar {Si,No,NS}?" crlf)
    (retract ?respuesta)
    (assert (programar (read)))
)

;;;Comenzamos con los consejos utilizando la plantilla que se nos proporciona en el guion.
(defrule consejoIC
    (hardware Si)
=>
    (assert (Consejo Ingenieria_de_Computadores "te gusta el hardware" "Experto seguro"))
)

(defrule consejoTI
    (hardware No)
    (programar No)
=>
    (assert (Consejo Tecnologias_de_la_Informacion "no te gusta el hardware ni programar" "Experto seguro"))
)

(defrule consejoCSI
    (hardware No)
    (programar Si)
    (mates Si)
    (or (nota Alta) (nota Media))
=>
    (assert (Consejo Computacion_y_Sistemas_Inteligentes "te gusta programar y las mates, y tus notas son altas" "Experto seguro"))
)

(defrule consejoTI2
    (hardware No)
    (programar Si)
    (mates Si)
    (nota Baja)
=>
    (assert (Consejo Tecnologias_de_la_Informacion "te gusta programar y las mates, pero tus notas son algo bajas, te recomiendo TI porque creo que CSI puede serte complicado" "Experto seguro"))
)

(defrule consejoSI
    (hardware No)
    (programar Si)
    (mates No)
    (trabajo Docencia)
=>
    (assert (Consejo Sistemas_de_Informacion "te gusta programar pero no las mates o el hardware, si en un futuro quieres ser docente te recomiendo SI" "Experto seguro"))
)

(defrule consejoIS
    (hardware No)
    (programar Si)
    (mates No)
    (trabajo ~Docencia)
=>
    (assert (Consejo Ingenieria_del_Software "te gusta programar pero no las mates, y no quieres ser docente por lo tanto te recomendaria IS" "Experto seguro"))
)

;;;Cuando hay NS no sabia que hacer asi que lo que hago es que el programa te pueda dar mas de una recomendacion por si cambia ese NS en un futuro
(defrule consejoNSHardware
    (hardware NS)
    (programar No)
=>
    (assert (Consejo Tecnologias_de_la_Informacion "si termina no gustandote hardware(TOC, AC, EC, ISE) no te recomendaria IC" "Experto inseguro con dos opciones"))
    (assert (Consejo Ingenieria_de_Computadores "si al contrario termina gustandote te recomendaria IC" "Experto inseguro con dos opciones"))
)

(defrule consejoNSHardware2
    (hardware NS)
    (programar ~No)
    (mates Si)
=>
    (assert (Consejo Computacion_y_Sistemas_Inteligentes "si termina no gustandote hardware(TOC, AC, EC, ISE) pero tienes buenas notas" "Experto inseguro con tres opciones"))
    (assert (Consejo Tecnologias_de_la_Informacion "si termina no gustandote hardware(TOC, AC, EC, ISE) pero no tienes buenas notas deberias evitar CSI" "Experto inseguro con tres opciones"))
    (assert (Consejo Ingenieria_de_Computadores "si al contrario termina gustandote te recomendaria IC" "Experto inseguro con tres opciones"))
)

(defrule consejoNSHardware3
    (hardware NS)
    (programar ~No)
    (mates No)
=>
    (assert (Consejo Sistemas_de_Informacion "si termina no gustandote hardware(TOC, AC, EC, ISE) pero quieres ser docente" "Experto inseguro con tres opciones"))
    (assert (Consejo Ingenieria_del_Software "si termina no gustandote hardware(TOC, AC, EC, ISE) pero no quieres ser docente" "Experto inseguro con tres opciones"))
    (assert (Consejo Ingenieria_de_Computadores "si al contrario termina gustandote te recomendaria IC" "Experto inseguro con tres opciones"))
)

(defrule consejoNSMates
    (mates NS)
=>
    (assert (Consejo Computacion_y_Sistemas_Inteligentes "si terminan gustandote las mates y tienes buenas notas te sera sencillo" "Experto inseguro con cuatro opciones"))
    (assert (Consejo Tecnologias_de_la_Informacion "si terminan gustandote las mates y pero no tienes las mejores notas te sera  mas sencillo que CSI" "Experto inseguro con cuatro opciones"))
    (assert (Consejo Sistemas_de_Informacion "si no terminan gustandote las mates y quieres dedicarte a la docencia" "Experto inseguro con cuatro opciones"))
    (assert (Consejo Ingenieria_del_Software "si no terminan gustandote las mates y no quieres ser docente" "Experto inseguro con cuatro opciones"))

)

;;;Para los consejos usamos el hecho que se nos recomienda en el guion
(defrule showConsejo
    (Rama ?r)
    (Consejo ?r ?motivo ?apodo)
=>
    (printout t ?apodo " te recomienda la mencion de " ?r ". Porque " ?motivo crlf)
)

;;;Es imposible saber algunas ramificaciones, si los datos que introduces no corresponden con las reglas que he creado mostraré este mensaje.
(defrule showConsejoNulo
    (not(Consejo ?r ?motivo ?apodo))
=>
    (printout t "No te puedo recomendar ninguna rama con las respuestas que me has dado." crlf)
)