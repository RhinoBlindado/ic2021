; [CASTELLANO]
; Practica 5.1: Razonamiento por Defecto en CLIPS
; Asignatura: Ingenieria del Conocimiento
; Autor: Valentino Lugli (Github: @RhinoBlindado)
; Fecha: Mayo, Junio 2021

; [ENGLISH]
; Practice 5.1: Default Reasoning with CLIPS
; Course: Knowledge Engineering
; Author: Valentino Lugli (Github: @RhinoBlindado)
; Date: May, June 2021


; Valores de Certeza
; (ave ?x) representa "?x es un ave "
; (animal ?x) representa "?x es un animal"
; (vuela ?x si|no seguro|por_defecto) representa
;                "?x vuela si|no con esa certeza"


; ##########################
; # HECHOS
; ##########################

; Las aves y los mamíferos son animales
;Los gorriones, las palomas, las águilas y los pingüinos son aves
;La vaca, los perros y los caballos son mamíferos
;Los pingüinos no vuelan
(deffacts datos
    (ave gorrion) 
    (ave paloma) 
    (ave aguila) 
    (ave pinguino)
    (mamifero vaca) 
    (mamifero perro) 
    (mamifero caballo)
    (vuela pinguino no seguro) 
)

; ##########################
; # REGLAS
; ##########################

; Las aves son animales
(defrule aves_son_animales
    (ave ?x)
        =>
    (assert (animal ?x))
    (bind ?expl (str-cat "sabemos que un " ?x " es un animal porque las aves son un tipo de animal"))
    (assert (explicacion animal ?x ?expl)) 
)
; añadimos un hecho que contiene la explicación de la deducción

; Los mamiferos son animales (A3)
(defrule mamiferos_son_animales
    (mamifero ?x)
        =>
    (assert (animal ?x))
    (bind ?expl (str-cat "Sabemos que un " ?x " es un animal porque los mamiferos son un tipo de animal"))
    (assert (explicacion animal ?x ?expl)) 
)
; añadimos un hecho que contiene la explicación de la deducción

;; Casi todos las aves vuela --> puedo asumir por defecto que las aves vuelan
; Asumimos por defecto
(defrule ave_vuela_por_defecto
    (declare (salience -1)) ; para disminuir probabilidad de añadir erróneamente
    (ave ?x)
        =>
    (assert (vuela ?x si por_defecto))
    (bind ?expl (str-cat "asumo que un " ?x " vuela, porque casi todas las aves vuelan"))
    (assert (explicacion vuela ?x ?expl)) 
)

; Retractamos cuando hay algo en contra
(defrule retracta_vuela_por_defecto
    (declare (salience 1)) ; para retractar antes de inferir cosas erroneamente
    ?f<- (vuela ?x ?r por_defecto)
    ?g<- (explicacion vuela ?x ?)
    (vuela ?x ?s seguro)
        =>
    (retract ?f)
    (retract ?g)
    (bind ?expl (str-cat "retractamos que un " ?x " " ?r " vuela por defecto, porque sabemos seguro que " ?x " " ?s " vuela"))
    (assert (explicacion retracta_vuela ?x ?expl)) 
)
;;; COMETARIO: esta regla también elimina los por defecto cuando ya esta seguro

;;; La mayor parte de los animales no vuelan --> puede interesarme asumir por defecto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;que un animal no va a volar
(defrule mayor_parte_animales_no_vuelan
    (declare (salience -2)) ;;;; es mas arriesgado, mejor después de otros razonamientos
    (animal ?x)
    (not (vuela ?x ? ?))
        =>
    (assert (vuela ?x no por_defecto))
    (bind ?expl (str-cat "asumo que " ?x " no vuela, porque la mayor parte de los animales no vuelan"))
    (assert (explicacion vuela ?x ?expl)) 
)


; ##########################
; # EJERCICIO
; ##########################

;   Completar esta base de conocimiento para que el sistema pregunte 
;   que de qué animal esta interesado en obtener información sobre si
;   vuela y:

(defrule startUp
    (declare (salience -10))
        =>
    (printout t crlf crlf"--- Practica 5.1: Reglas por Defecto ---" crlf)
    (printout t crlf "--- PREGUNTA ---" crlf ">>Escribe el nombre de un animal para saber si vuela o no." crlf ">")
    (assert (inputAnimal ( lowcase(read) ) ) )
)

;   -   Si es uno de los recogidos en el conocimiento indique si vuela o no
(defrule checkAnimal
    ; El animal que se ha introducido está en la base de conocimientos.
    (inputAnimal ?specimen)
    (animal ?specimen)
    ; Se toman las explicaciones y se imprimen en pantalla.
    (explicacion animal ?specimen ?expl1)
    (explicacion vuela|retracta_vuela ?specimen ?expl2)
    =>
    (printout t crlf "### RESPUESTA ###" crlf ">>" ?expl1 "; " ?expl2 "." crlf)
    (assert (animalIsKnown))
)

(defrule checkAnimal_2
    ; Cuando se ha introducido un nuevo animal que no es mamifero o ave, se dispara esta regla.
    (inputAnimal ?specimen)
    (animal ?specimen)
    ; No posee la explicación del animal pero si la de que si vuela o no.
    (not(explicacion animal ?specimen ?expl1))
    (explicacion vuela|retracta_vuela ?specimen ?expl2)
    =>
    (printout t crlf "### RESPUESTA ###" crlf ">>" ?expl2 "." crlf)
    (assert (animalIsKnown))
)

;   -  Si no es uno de los recogidos pregunte si es un ave o un mamífero y según la respuesta indique si vuela o no.
;   -  Si no se sabe si es un mamífero o un ave también responda según el razonamiento por defecto indicado.
(defrule askAnimal_notKnown
    (not(animalIsKnown))
    (inputAnimal ?specimen)
    =>
    (printout t crlf ">>Aviso: No tengo el animal '"?specimen"' en mi base de conocimiento." crlf)
    (printout t      ">>Que tipo de animal es? " crlf ">>(Ave | Mamimero | Otro)" crlf ">")
    (assert (inputAnimalType ( lowcase(read) ) ) )
)

(defrule checkAnimal_notKnown
    (inputAnimalType ?type)
    (inputAnimal ?specimen)
    =>
    ; Dependiendo de la entrada, se indica que el animal es un mamífero, un ave o que es otra clase de animal.
    (if (eq ?type mamifero) then
        (assert (mamifero ?specimen))
        else (if (eq ?type ave) then
            (assert (ave ?specimen))
        )
        else
        (assert (animal ?specimen))
    )
)