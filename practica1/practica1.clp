; PRACTICA 1: Ejercicio de la Introducción a los sistemas basados en reglas
; Autor: Valentino Lugli - Marzo 2021


; DEFINICION HECHOS
;  Listado familiar
(deffacts family
   (hombre Pasquale)
   (hombre David)
   (hombre Darrell)
   (hombre DarrellJr)
   (hombre Fabio)
   (hombre Argilio)
   (hombre Stefano)
   (hombre Emmanuelle)
   (hombre Florideo)
   (hombre Roman)
   (hombre Constantino)
   (hombre Luciano)
   (hombre Alvaro)
   (hombre Glauco)
   (hombre Livio)
   (hombre Valentino)
   (mujer Olga)
   (mujer Nilde)
   (mujer Jeanette)
   (mujer Elisa)
   (mujer Fiorella)
   (mujer Rosa)
   (mujer Samara)
   (mujer Valentina)
   (mujer Veronica)
   (mujer Liliana)
   (mujer Zuldy)
   (mujer Daniela)
   (mujer CarmenLiliana)
)

;  Plantilla para relaciones
(deftemplate Relacion 
  (slot tipo (type SYMBOL) (allowed-symbols HIJO PADRE ABUELO NIETO HERMANO ESPOSO PRIMO TIO SOBRINO CUNIADO YERNO SUEGRO))
  (slot sujeto)
  (slot objeto))

(deffacts relations
   (Relacion (tipo HIJO) (sujeto Nilde) (objeto Pasquale))

   (Relacion (tipo HIJO) (sujeto Elisa) (objeto Pasquale))

   (Relacion (tipo HIJO) (sujeto Fabio) (objeto Pasquale))
   (Relacion (tipo HIJO) (sujeto Valentina) (objeto Fabio))

   (Relacion (tipo HIJO) (sujeto Argilio) (objeto Pasquale))
   (Relacion (tipo HIJO) (sujeto Stefano) (objeto Argilio))
   (Relacion (tipo HIJO) (sujeto Emmanuelle) (objeto Argilio))

   (Relacion (tipo HIJO) (sujeto Florideo) (objeto Pasquale))
   (Relacion (tipo HIJO) (sujeto Fiorella) (objeto Florideo))

   (Relacion (tipo HIJO) (sujeto Roman) (objeto Pasquale))
   (Relacion (tipo HIJO) (sujeto Constantino) (objeto Roman))
   (Relacion (tipo HIJO) (sujeto Luciano) (objeto Roman))

   (Relacion (tipo HIJO) (sujeto Glauco) (objeto Alvaro))
   (Relacion (tipo HIJO) (sujeto Valentino) (objeto Glauco))

   (Relacion (tipo HIJO) (sujeto Livio) (objeto Alvaro))
   (Relacion (tipo HIJO) (sujeto Samara) (objeto Livio))

   (Relacion (tipo HIJO) (sujeto David) (objeto Darrell))
   (Relacion (tipo HIJO) (sujeto DarrellJr) (objeto Darrell))
   (Relacion (tipo HIJO) (sujeto Jeanette) (objeto Darrell))

   (Relacion (tipo ESPOSO) (sujeto Pasquale) (objeto Olga))
   (Relacion (tipo ESPOSO) (sujeto Alvaro) (objeto Rosa))
   (Relacion (tipo ESPOSO) (sujeto Darrell) (objeto Nilde))
   (Relacion (tipo ESPOSO) (sujeto Glauco) (objeto Elisa))
   (Relacion (tipo ESPOSO) (sujeto Fabio) (objeto Veronica))
   (Relacion (tipo ESPOSO) (sujeto Argilio) (objeto Liliana))
   (Relacion (tipo ESPOSO) (sujeto Florideo) (objeto Zuldy))
   (Relacion (tipo ESPOSO) (sujeto Roman) (objeto Daniela))
   (Relacion (tipo ESPOSO) (sujeto Livio) (objeto CarmenLiliana))
)
;   Dualidad de relaciones
(deffacts duales
(dual HIJO PADRE) (dual ABUELO NIETO) (dual HERMANO HERMANO) 
(dual ESPOSO ESPOSO) 
(dual PRIMO PRIMO) (dual TIO SOBRINO) 
(dual CUNIADO CUNIADO) 
(dual YERNO SUEGRO))

;   Relaciones deducidas
(deffacts compuestos
(comp HIJO HIJO NIETO) (comp PADRE PADRE ABUELO) (comp ESPOSO PADRE PADRE)(comp HERMANO PADRE TIO) (comp HERMANO ESPOSO CUNIADO) (comp ESPOSO HIJO YERNO) (comp ESPOSO HERMANO CUNIADO) (comp HIJO PADRE HERMANO) (comp ESPOSO CUNIADO CUNIADO) (comp ESPOSO TIO TIO)  (comp HIJO TIO PRIMO)  ) 

;   Añadiendo relaciones en femenino para cuando aplique
(deffacts femenino
(femenino HIJO HIJA) (femenino PADRE MADRE) (femenino ABUELO ABUELA) (femenino NIETO NIETA) (femenino HERMANO HERMANA) (femenino ESPOSO ESPOSA) (femenino PRIMO PRIMA) (femenino TIO TIA) (femenino SOBRINO SOBRINA) (femenino CUNIADO CUNIADA) (femenino YERNO NUERA) (femenino SUEGRO SUEGRA)) 


; REGLAS 
;  La dualidad es simetrica: si r es dual de t, t es dual de r. Por eso solo metimos como hecho la dualidad en un sentidos, pues en el otro lo podiamos deducir con esta regla
(defrule autodualidad
   (razonar)
   (dual ?r ?t)
=> 
   (assert (dual ?t ?r))
)

;  Si  x es R de y, entonces y es dualdeR de x
(defrule dualidad
   (razonar)
   (Relacion (tipo ?r) (sujeto ?x) (objeto ?y))
   (dual ?r ?t)
=> 
   (assert (Relacion (tipo ?t) (sujeto ?y) (objeto ?x)))
)


;;;; Si  y es R de x, y x es T de z entonces y es RoT de z
;;;; a�adimos que z e y sean distintos para evitar que uno resulte hermano de si mismo y cosas asi.

(defrule composicion
   (razonar)
   (Relacion (tipo ?r) (sujeto ?y) (objeto ?x))
   (Relacion (tipo ?t) (sujeto ?x) (objeto ?z))
   (comp ?r ?t ?u)
   (test (neq ?y ?z))
=> 
;  (printout t "Composicion: Tipo:" ?u ", Sujeto: " ?y ", Objeto: " ?z crlf)
   (assert (Relacion (tipo ?u) (sujeto ?y) (objeto ?z))))

;;;;; Como puede deducir que tu hermano es tu cu�ado al ser el esposo de tu cu�ada, eliminamos los cu�ados que sean hermanos

(defrule limpiacuniados
   (Relacion (tipo HERMANO) (sujeto ?x) (objeto ?y))
   ?f <- (Relacion (tipo CUNIADO) (sujeto ?x) (objeto ?y))
=>
	(retract ?f) 
)

;  Inicio del programa
(defrule startUp
   ; Para que aparezcan en orden se utiliza el salience.
   (declare (salience 6)) 
=>
   (printout t crlf"--- PRACTICA 1: Determinar relacion entre familiares ---" crlf)
   (printout t ">LISTA DE FAMILIARES: " crlf)

   (assert (printFamM))
)

;  Imprimir los hombres y luego las mujeres.
(defrule printFamMale
   (declare (salience 5)) 
   (printFamM)
   (hombre ?m)
   =>
   (printout t "> " ?m crlf)
   (assert (printFamF))
   
)

(defrule printFamFemale
   (printFamF)
   (mujer ?m)
   =>
   (printout t "> " ?m crlf)
   (assert (firstQuestion))
)

;  Realizar primera pregunta.
(defrule pregunta1
   ?rule1 <- (firstQuestion)
   ?rule2 <- (printFamM)
   ?rule3 <- (printFamF)
=>

   (printout t ">Escribir el nombre de la persona a determinar su relacion con otras:" crlf ">>")
   (assert (primerapersona (read)))
   (printout t ">RELACIONES DISPONIBLES: " crlf)
   (retract ?rule1)
   (retract ?rule2)
   (retract ?rule3)
)

; Verificar que la persona esté en la base de conocimientos...
(defrule checkPersona
   (declare (salience 3))
   (primerapersona ?person)
   (or (hombre ?person) (mujer ?person))
   =>
   (assert (personExists))
)
; Si no está, volver a preguntar.
(defrule personDoesNotExist
   (not (personExists))
   ?rule <- (primerapersona ?person)
   =>
   (printout t ">--ERROR--: " ?person " no se encuentra en la base de conocimientos. Vuelve a intentar." crlf)
   (printout t ">LISTA DE FAMILIARES: " crlf)
   (retract ?rule)
   (assert (printFamM))
)

; Si está, continuar.
(defrule personDoesExists
   (personExists)
   =>
   (assert (printR))
)

;  Imprimir relaciones disponibles.
(defrule printRelation
   (declare (salience 2))
   (printR)
   (femenino ?r1 ?r2)
   =>
   (printout t "> " ?r1 ", " ?r2 crlf)
   (assert (secondQuestion))
)

;  Solicitar la relacion de parentesco 
(defrule pregunta2
   (declare (salience 1))
   ?rule1 <- (secondQuestion)
   ?rule2 <- (printR)
      =>
   (printout t ">Escribir una relacion de parentesco para determinar familiares que lo cumplen:" crlf ">>")
   (assert (relationship (read)))
   (retract ?rule1)
   (retract ?rule2)
)

;  Verificar que es una relación válida.
(defrule isValidRelation
   (relationship ?r)
   (or (femenino ?r ?) (femenino ? ?r))
   =>
   (assert (razonar))

)
;  Si no lo es, volver a preguntar...
(defrule notValidRelation
   (not (razonar))
   (relationship ?r)
   =>
   (assert (printR))
   (printout t ">--ERROR--: La relacion " ?r " no es valida. Intente de nuevo." crlf)
   (printout t ">RELACIONES DISPONIBLES:" crlf)
)

;  Obtener familiares para la relacion, se diferencian los generos. Es decir, si se escribe NIETO solamente se obtendran los nietos varones, si se escribe NIETA solo
;  se obtendran las nietas mujeres.
(defrule famRelationMale
   ; Se mantiene el razonar ya que tambien sirve para darle precedencia a esta regla sobre el resto de reglas anteriores a 'startUp'.
   (razonar)
   (primerapersona ?x)
   (relationship ?r)

   ; Dada la primera persona ?X y una relacion ?r; obtener el sujeto ?y de esa relacion.
   (Relacion (tipo ?r) (sujeto ?y) (objeto ?x))

   ; "Filtrar" para que sea hombre.
   (hombre ?y)
      =>
   (printout t ">" ?y " es " ?r " de " ?x crlf)

   ; Si se ha encontrado al menos un familiar, entonces asertar este hecho. Se utilizara como un booleano.
   (assert (encontrado)) 
)

(defrule famRelationFemale
   (razonar)
   (primerapersona ?x)
   (relationship ?r)
   (Relacion (tipo ?t) (sujeto ?y) (objeto ?x))   
   ; Se "filtra" para que sea el sujeto mujer.
   (mujer ?y)
   (femenino ?t ?r)
      =>
   (printout t ">" ?y " es " ?r " de " ?x crlf)

   ; Idem
   (assert (encontrado))
)

(defrule famRelationNotFound
   ; Si no se ha asertado este hecho entonces activar esta regla, de esta manera si no se consiguen relaciones se dispara la regla.
   (not (encontrado))
   (primerapersona ?x)
   (relationship ?r)
      =>
   (printout t ">" ?x " no posee ninguna relacion de tipo " ?r crlf)
)