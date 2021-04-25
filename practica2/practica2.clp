;;;;;;; JUGADOR DE 4 en RAYA ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;; Version de 4 en raya clásico: Tablero de 6x7, donde se introducen fichas por arriba
;;;;;;;;;;;;;;;;;;;;;;; y caen hasta la posicion libre mas abajo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Hechos para representar un estado del juego

;;;;;;; (Turno M|J)   representa a quien corresponde el turno (M maquina, J jugador)
;;;;;;; (Tablero Juego ?i ?j _|M|J) representa que la posicion i,j del tablero esta vacia (_), o tiene una ficha propia (M) o tiene una ficha del jugador humano (J)

;;;;;;;;;;;;;;;; Hechos para representar estado del analisis
;;;;;;; (Tablero Analisis Posicion ?i ?j _|M|J) representa que en el analisis actual la posicion i,j del tablero esta vacia (_), o tiene una ficha propia (M) o tiene una ficha del jugador humano (J)
;;;;;;; (Sondeando ?n ?i ?c M|J)  ; representa que estamos analizando suponiendo que la ?n jugada h sido ?i ?c M|J
;;;

;;;;;;;;;;;;; Hechos para representar una jugadas

;;;;;;; (Juega M|J ?columna) representa que la jugada consiste en introducir la ficha en la columna ?columna 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIALIZAR ESTADO


(deffacts Estado_inicial
(Tablero Juego 1 1 _) (Tablero Juego 1 2 _) (Tablero Juego 1 3 _) (Tablero Juego  1 4 _) (Tablero Juego  1 5 _) (Tablero Juego  1 6 _) (Tablero Juego  1 7 _)
(Tablero Juego 2 1 _) (Tablero Juego 2 2 _) (Tablero Juego 2 3 _) (Tablero Juego 2 4 _) (Tablero Juego 2 5 _) (Tablero Juego 2 6 _) (Tablero Juego 2 7 _)
(Tablero Juego 3 1 _) (Tablero Juego 3 2 _) (Tablero Juego 3 3 _) (Tablero Juego 3 4 _) (Tablero Juego 3 5 _) (Tablero Juego 3 6 _) (Tablero Juego 3 7 _)
(Tablero Juego 4 1 _) (Tablero Juego 4 2 _) (Tablero Juego 4 3 _) (Tablero Juego 4 4 _) (Tablero Juego 4 5 _) (Tablero Juego 4 6 _) (Tablero Juego 4 7 _)
(Tablero Juego 5 1 _) (Tablero Juego 5 2 _) (Tablero Juego 5 3 _) (Tablero Juego 5 4 _) (Tablero Juego 5 5 _) (Tablero Juego 5 6 _) (Tablero Juego 5 7 _)
(Tablero Juego 6 1 _) (Tablero Juego 6 2 _) (Tablero Juego 6 3 _) (Tablero Juego 6 4 _) (Tablero Juego 6 5 _) (Tablero Juego 6 6 _) (Tablero Juego 6 7 _)
(Jugada 0)
)

(defrule Elige_quien_comienza
=>
(printout t "Quien quieres que empieze: (escribre M para la maquina o J para empezar tu) ")
(assert (Turno (read)))
)

;;;;;;;;;;;;;;;;;;;;;;; MUESTRA POSICION ;;;;;;;;;;;;;;;;;;;;;;;
(defrule muestra_posicion
(declare (salience 10))
(muestra_posicion)
(Tablero Juego 1 1 ?p11) (Tablero Juego 1 2 ?p12) (Tablero Juego 1 3 ?p13) (Tablero Juego 1 4 ?p14) (Tablero Juego 1 5 ?p15) (Tablero Juego 1 6 ?p16) (Tablero Juego 1 7 ?p17)
(Tablero Juego 2 1 ?p21) (Tablero Juego 2 2 ?p22) (Tablero Juego 2 3 ?p23) (Tablero Juego 2 4 ?p24) (Tablero Juego 2 5 ?p25) (Tablero Juego 2 6 ?p26) (Tablero Juego 2 7 ?p27)
(Tablero Juego 3 1 ?p31) (Tablero Juego 3 2 ?p32) (Tablero Juego 3 3 ?p33) (Tablero Juego 3 4 ?p34) (Tablero Juego 3 5 ?p35) (Tablero Juego 3 6 ?p36) (Tablero Juego 3 7 ?p37)
(Tablero Juego 4 1 ?p41) (Tablero Juego 4 2 ?p42) (Tablero Juego 4 3 ?p43) (Tablero Juego 4 4 ?p44) (Tablero Juego 4 5 ?p45) (Tablero Juego 4 6 ?p46) (Tablero Juego 4 7 ?p47)
(Tablero Juego 5 1 ?p51) (Tablero Juego 5 2 ?p52) (Tablero Juego 5 3 ?p53) (Tablero Juego 5 4 ?p54) (Tablero Juego 5 5 ?p55) (Tablero Juego 5 6 ?p56) (Tablero Juego 5 7 ?p57)
(Tablero Juego 6 1 ?p61) (Tablero Juego 6 2 ?p62) (Tablero Juego 6 3 ?p63) (Tablero Juego 6 4 ?p64) (Tablero Juego 6 5 ?p65) (Tablero Juego 6 6 ?p66) (Tablero Juego 6 7 ?p67)
=>
(printout t crlf)
(printout t ?p11 " " ?p12 " " ?p13 " " ?p14 " " ?p15 " " ?p16 " " ?p17 crlf)
(printout t ?p21 " " ?p22 " " ?p23 " " ?p24 " " ?p25 " " ?p26 " " ?p27 crlf)
(printout t ?p31 " " ?p32 " " ?p33 " " ?p34 " " ?p35 " " ?p36 " " ?p37 crlf)
(printout t ?p41 " " ?p42 " " ?p43 " " ?p44 " " ?p45 " " ?p46 " " ?p47 crlf)
(printout t ?p51 " " ?p52 " " ?p53 " " ?p54 " " ?p55 " " ?p56 " " ?p57 crlf)
(printout t ?p61 " " ?p62 " " ?p63 " " ?p64 " " ?p65 " " ?p66 " " ?p67 crlf)
(printout t  crlf)
)


;;;;;;;;;;;;;;;;;;;;;;; RECOGER JUGADA DEL CONTRARIO ;;;;;;;;;;;;;;;;;;;;;;;
(defrule mostrar_posicion
(declare (salience 9999))
(Turno J)
=>
(assert (muestra_posicion))
)

(defrule jugada_contrario
?f <- (Turno J)
=>
(printout t "en que columna introduces la siguiente ficha? ")
(assert (Juega J (read)))
(retract ?f)
)

(defrule juega_contrario_check_entrada_correcta
(declare (salience 1))
?f <- (Juega J ?c)
(test (and (neq ?c 1) (and (neq ?c 2) (and (neq ?c 3) (and (neq ?c 4) (and (neq ?c 5) (and (neq ?c 6) (neq ?c 7))))))))
=>
(printout t "Tienes que indicar un numero de columna: 1,2,3,4,5,6 o 7" crlf)
(retract ?f)
(assert (Turno J))
)

(defrule juega_contrario_check_columna_libre
(declare (salience 1))
?f <- (Juega J ?c)
(Tablero Juego 1 ?c ?X) 
(test (neq ?X _))
=>
(printout t "Esa columna ya esta completa, tienes que jugar en otra" crlf)
(retract ?f)
(assert (Turno J))
)

(defrule juega_contrario_actualiza_estado
?f <- (Juega J ?c)
?g <- (Tablero Juego ?i ?c _)
(Tablero Juego ?j ?c ?X) 
(test (= (+ ?i 1) ?j))
(test (neq ?X _))
=>
(retract ?f ?g)
(assert (Turno M) (Tablero Juego ?i ?c J))
)

(defrule juega_contrario_actualiza_estado_columna_vacia
?f <- (Juega J ?c)
?g <- (Tablero Juego 6 ?c _)
=>
(retract ?f ?g)
(assert (Turno M) (Tablero Juego 6 ?c J))
)


;;;;;;;;;;; ACTUALIZAR  ESTADO TRAS JUGADA DE CLISP ;;;;;;;;;;;;;;;;;;

(defrule juega_clisp_actualiza_estado
?f <- (Juega M ?c)
?g <- (Tablero Juego ?i ?c _)
(Tablero Juego ?j ?c ?X) 
(test (= (+ ?i 1) ?j))
(test (neq ?X _))
=>
(retract ?f ?g)
(assert (Turno J) (Tablero Juego ?i ?c M))
)

(defrule juega_clisp_actualiza_estado_columna_vacia
?f <- (Juega M ?c)
?g <- (Tablero Juego 6 ?c _)
=>
(retract ?f ?g)
(assert (Turno J) (Tablero Juego 6 ?c M))
)

;;;;;;;;;;; CLISP JUEGA SIN CRITERIO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule elegir_jugada_aleatoria
(declare (salience -9998))
?f <- (Turno M)
=>
(assert (Jugar (random 1 7)))
(retract ?f)
)

(defrule comprobar_posible_jugada_aleatoria
?f <- (Jugar ?c)
(Tablero Juego 1 ?c M|J)
=>
(retract ?f)
(assert (Turno M))
)

(defrule clisp_juega_sin_criterio
(declare (salience -9999))
?f<- (Jugar ?c)
=>
(printout t "JUEGO en la columna (sin criterio) " ?c crlf)
(retract ?f)
(assert (Juega M ?c))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;  Comprobar si hay 4 en linea ;;;;;;;;;;;;;;;;;;;;;

(defrule cuatro_en_linea_horizontal
(declare (salience 9999))
(Tablero ?t ?i ?c1 ?jugador)
(Tablero ?t ?i ?c2 ?jugador) 
(test (= (+ ?c1 1) ?c2))
(Tablero ?t ?i ?c3 ?jugador)
(test (= (+ ?c1 2) ?c3))
(Tablero ?t ?i ?c4 ?jugador)
(test (= (+ ?c1 3) ?c4))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador horizontal ?i ?c1))
)

(defrule cuatro_en_linea_vertical
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i1 ?c ?jugador)
(Tablero ?t ?i2 ?c ?jugador)
(test (= (+ ?i1 1) ?i2))
(Tablero ?t ?i3 ?c  ?jugador)
(test (= (+ ?i1 2) ?i3))
(Tablero ?t ?i4 ?c  ?jugador)
(test (= (+ ?i1 3) ?i4))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador vertical ?i1 ?c))
)

(defrule cuatro_en_linea_diagonal_directa
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i ?c ?jugador)
(Tablero ?t ?i1 ?c1 ?jugador)
(test (= (+ ?i 1) ?i1))
(test (= (+ ?c 1) ?c1))
(Tablero ?t ?i2 ?c2  ?jugador)
(test (= (+ ?i 2) ?i2))
(test (= (+ ?c 2) ?c2))
(Tablero ?t ?i3 ?c3  ?jugador)
(test (= (+ ?i 3) ?i3))
(test (= (+ ?c 3) ?c3))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador diagonal_directa ?i ?c))
)

(defrule cuatro_en_linea_diagonal_inversa
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i ?c ?jugador)
(Tablero ?t ?i1 ?c1 ?jugador)
(test (= (+ ?i 1) ?i1))
(test (= (- ?c 1) ?c1))
(Tablero ?t ?i2 ?c2  ?jugador)
(test (= (+ ?i 2) ?i2))
(test (= (- ?c 2) ?c2))
(Tablero ?t ?i3 ?c3  ?jugador)
(test (= (+ ?i 3) ?i3))
(test (= (- ?c 3) ?c3))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador diagonal_inversa ?i ?c))
)

;;;;;;;;;;;;;;;;;;;; DESCUBRE GANADOR
(defrule gana_fila
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador horizontal ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la fila " ?i crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_columna
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador vertical ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la columna " ?c crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_diagonal_directa
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador diagonal_directa ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la diagonal que empieza la posicion " ?i " " ?c   crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_diagonal_inversa
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador diagonal_inversa ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la diagonal hacia arriba que empieza la posicin " ?i " " ?c   crlf)
(retract ?f)
(assert (muestra_posicion))
) 


;;;;;;;;;;;;;;;;;;;;;;;  DETECTAR EMPATE

(defrule empate
(declare (salience -9999))
(Turno ?X)
(Tablero Juego 1 1 M|J)
(Tablero Juego 1 2 M|J)
(Tablero Juego 1 3 M|J)
(Tablero Juego 1 4 M|J)
(Tablero Juego 1 5 M|J)
(Tablero Juego 1 6 M|J)
(Tablero Juego 1 7 M|J)
=>
(printout t "EMPATE! Se ha llegado al final del juego sin que nadie gane" crlf)
)

;;;;;;;;;;;;;;;;;;;;;; CONOCIMIENTO EXPERTO ;;;;;;;;;;
;;;;; ¡¡¡¡¡¡¡¡¡¡ Añadir conocimiento para que juege como vosotros jugariais !!!!!!!!!!!!

; [CASTELLANO]
; Practica 2: Insertar conocimiento experto para jugar 4 en raya.
; Asignatura: Ingenieria del Conocimiento
; Autor: Valentino Lugli (Github: @RhinoBlindado)
; Fecha: Marzo, Abril 2021

; [ENGLISH]
; Practice 2: Insert expert knowledge to play Connect Four.
; Course: Knowledge Engineering
; Author: Valentino Lugli (Github: @RhinoBlindado)
; Date: March, April 2021

; CHEQUEO DE ESTADO
; 1) Crear reglas para que el sistema deduzca la posición siguiente y anterior a una posición

;   Regla que obtiene la posición siguiente horizontal a una ficha; es decir por la derecha.
(defrule nextPos_h
    ; Obtener del tablero de juego la fila y columna de un jugador.
    (Tablero Juego ?f ?c ?player)
    (test (or (eq ?player M) (eq ?player J)))
    ; Comprobar que la columna es menor o igual a 7, es decir que está dentro del tablero.
    (test (<= (+ 1 ?c) 7))
        =>
    (assert (nextPos H ?f (+ 1 ?c) ?player))
)

;   Obtiene la posición siguiente diagonal directa por la derecha abajo.
(defrule nextPos_d1
    (Tablero Juego ?f ?c ?player)
    (test (or (eq ?player M) (eq ?player J)))
    (test (<= (+ 1 ?f) 6))
    (test (<= (+ 1 ?c) 7))
        =>
    (assert (nextPos D1 (+ 1 ?f) (+ 1 ?c) ?player)) ; Diagonal directa
)

;   Idem por diagonal inversa derecha arriba.
(defrule nextPos_d2
    (Tablero Juego ?f ?c ?player)
    (test (or (eq ?player M) (eq ?player J)))
    (test (>= (- ?f 1) 1))
    (test (<= (+ 1 ?c) 7))
        =>
    (assert (nextPos D2 (- ?f 1) (+ 1 ?c) ?player)) 
)

;   Obtiene la posición anterior, es decir, encima de una ficha.
(defrule prevPos_v
    (Tablero Juego ?f ?c ?player)
    (test (or (eq ?player M) (eq ?player J)))
    (test (>= (- ?f 1) 1))
        =>
    (assert (prevPos V (- ?f 1) ?c ?player))
)

;   Regla que obtiene la posición anterior horizontal a una ficha; es decir por la izquierda.
(defrule prevPos_h
    (Tablero Juego ?f ?c ?player)
    (test (or (eq ?player M) (eq ?player J)))
    (test (>= (- ?c 1) 1))
        =>
    (assert (prevPos H ?f (- ?c 1) ?player))
)

;   Obtienen los datos opuestos a sus contrapartes de nextPos.
(defrule prevPos_d1
    (Tablero Juego ?f ?c ?player)
    (test (or (eq ?player M) (eq ?player J)))
    (test (>= (- ?f 1) 1))
    (test (>= (- ?c 1) 1))
        =>

    (assert (prevPos D1 (- ?f 1) (- ?c 1) ?player))
)

(defrule prevPos_d2
    (Tablero Juego ?f ?c ?player)
    (test (or (eq ?player M) (eq ?player J)))
    (test (<= (+ 1 ?f) 6))
    (test (>= (- ?c 1) 1))
        =>
    (assert (prevPos D2 (+ ?f 1) (- ?c 1) ?player))
)

; 2) Crear reglas para que el sistema deduzca (y mantenga) donde caería una ficha si se juega en la columna c.
;   Esta regla detecta que el fondo del tablero está vacío.
(defrule wouldFall_empty
    (declare (salience 100))
    ; Se realiza en el turno de la Máquina.
    (Turno M)
    (Tablero Juego 6 ?c _)
        =>
    (assert (wouldFall 6 ?c))
)

;   Esta regla detecta que una ficha caería encima de una ficha puesta por el jugador o la máquina.
(defrule wouldFall
    (declare (salience 100))
    (Turno M)
    ; Si la posición del espacio en blanco está por encima que la ficha y es la misma columna, disparar regla.
    (Tablero Juego ?f ?c _)
    (Tablero Juego ?f1 ?c M|J)
    (test (= (+ ?f 1) ?f1))
        =>
    (assert (wouldFall ?f ?c))
)

; 3) Crear reglas para que el sistema deduzca que hay dos fichas de un mismo jugador en línea en una dirección y posiciones concretas
(defrule twoTokens_vertical
    (declare (salience 100))
    (Turno M)
    (Tablero Juego ?f1 ?c ?player)
    (Tablero Juego ?f2 ?c ?player)

    ; Filtrar para que sea solo jugador o maquina
    (test (or (eq ?player M) (eq ?player J)))

    ; Si son iguales es que hay conexion.
    (test (= (+ ?f1 1) ?f2))
        =>
    (assert (twoTokens V ?player ?f1 ?c ?f2 ?c))
)

(defrule twoTokens_horizontal
    (declare (salience 100))
    (Turno M)
    (Tablero Juego ?f ?c1 ?player)
    (Tablero Juego ?f ?c2 ?player)

    ; Filtrar para que sea solo jugador o maquina
    (test (or (eq ?player M) (eq ?player J)))

    ; Si son iguales es que hay conexion.
    (test (= (+ ?c1 1) ?c2))
        =>
    (assert (twoTokens H ?player ?f ?c1 ?f ?c2))
)

(defrule twoTokens_mainDiag
    (declare (salience 100))
    (Turno M)
    (Tablero Juego ?f1 ?c1 ?player)
    (Tablero Juego ?f2 ?c2 ?player)

    ; Filtrar para que sea solo jugador o maquina
    (test (or (eq ?player M) (eq ?player J)))

    ; Si son iguales es que hay conexion.
    (test (= (+ ?c1 1) ?c2))
    (test (= (+ ?f1 1) ?f2))
        =>
    (assert (twoTokens D1 ?player ?f1 ?c1 ?f2 ?c2))
)

(defrule twoTokens_secondDiag
    (declare (salience 100))
    (Turno M)
    (Tablero Juego ?f1 ?c1 ?player)
    (Tablero Juego ?f2 ?c2 ?player)

    ; Filtrar para que sea solo jugador o maquina
    (test (or (eq ?player M) (eq ?player J)))

    ; Si son iguales es que hay conexion.
    (test (= (+ ?c1 1) ?c2))
    (test (= (- ?f1 1) ?f2))
        =>
    (assert (twoTokens D2 ?player ?f1 ?c1 ?f2 ?c2))
)

; 4) Crear reglas para deducir que un jugador tiene 3 en línea en una dirección y posiciones concretas
(defrule threeTokensInLine_vertical
    (declare (salience 100))
    (Turno M)
    ; Si hay ya dos fichas verticales y se coloca una tercera encima...
    (twoTokens V ?player ?f1 ?c ?f2 ?c)
    (Tablero Juego ?f3 ?c ?player)

    ; Filtrar para que sea solo jugador o maquina.
    (test (or (eq ?player M) (eq ?player J)))

    ; Evaluar que existan 3 fichas en linea.
    (test (= (+ ?f1 2) ?f3))
        =>
    (assert (threeTokensInLine V ?player ?f1 ?c ?f3 ?c))
)

(defrule threeTokensInLine_horizontal
    (declare (salience 100))
    (Turno M)

    ; Se hace de esta manera porque puede suceder que _ X X o que X X _ , entonces esta regla captura esas dos posibilidades
    ; mientras que si se realiza con twoTokens de debe realizar dos reglas diferentes.
    (Tablero Juego ?f ?c1 ?player)
    (Tablero Juego ?f ?c2 ?player)
    (Tablero Juego ?f ?c3 ?player)

    ; Filtrar para que sea solo jugador o maquina.
    (test (or (eq ?player M) (eq ?player J)))

    ; Evaluar que existan 3 fichas en linea.
    (test (= (+ ?c1 1) ?c2))
    (test (= (+ ?c1 2) ?c3))
        =>
    (assert (threeTokensInLine H ?player ?f ?c1 ?f ?c3))
)

(defrule threeTokensInLine_mainDiag
    (declare (salience 100))
    (Turno M)

    ; Mismo argumento que para dos fichas horizontales, se ahorra código.
    (Tablero Juego ?f1 ?c1 ?player)
    (Tablero Juego ?f2 ?c2 ?player)
    (Tablero Juego ?f3 ?c3 ?player)

    ; Filtrar para que sea solo jugador o maquina.
    (test (or (eq ?player M) (eq ?player J)))

    ; Evaluar que existan 3 fichas en linea.
    (test (= (+ ?c1 1) ?c2))
    (test (= (+ ?c1 2) ?c3))
    (test (= (+ ?f1 1) ?f2))
    (test (= (+ ?f1 2) ?f3))
        =>
    (assert (threeTokensInLine D1 ?player ?f1 ?c1 ?f3 ?c3))
)

(defrule threeTokensInLine_inverseDiag
    (declare (salience 100))
    (Turno M)
    (Tablero Juego ?f1 ?c1 ?player)
    (Tablero Juego ?f2 ?c2 ?player)
    (Tablero Juego ?f3 ?c3 ?player)

    ; Filtrar para que sea solo jugador o maquina.
    (test (or (eq ?player M) (eq ?player J)))

    ; Evaluar que existan 3 fichas en linea.
    (test (= (- ?c1 1) ?c2))
    (test (= (- ?c1 2) ?c3))
    (test (= (+ ?f1 1) ?f2))
    (test (= (+ ?f1 2) ?f3))
        =>
    (assert (threeTokensInLine D2 ?player ?f1 ?c1 ?f3 ?c3))
)


; 5) Añadir reglas para que el sistema deduzca (y mantenga) que un jugador ganaría si jugase en una columna.
(defrule canWin_v
    (declare (salience 90))
    (Turno M)
    ; Si hay tres fichas alineadas y un espacio luego... entonces puede que gane.
    (threeTokensInLine V ?player ?f1 ?c ?f3 ?c)
    (Tablero Juego ?f4 ?c _)
    (test (= (- ?f1 1) ?f4))
    (test (or (eq ?player M) (eq ?player J)))
        =>
    (assert (canWin ?player ?c))
)

(defrule canWin_hLeft
    (declare (salience 90))
    (Turno M)
    (threeTokensInLine H ?player ?f ?c1 ?f ?c3)
    (wouldFall ?f ?c4)
    (test (= (+ ?c1 3) ?c4))
    (test (or (eq ?player M) (eq ?player J)))
        =>
    (assert (canWin ?player ?c4))
)

(defrule canWin_hRight
    (declare (salience 90))
    (Turno M)
    (threeTokensInLine H ?player ?f ?c1 ?f ?c3)
    (wouldFall ?f ?c4)
    (test (= (- ?c1 1) ?c4))
    (test (or (eq ?player M) (eq ?player J)))
        =>
    (assert (canWin ?player ?c4))
)

(defrule canWin_separated_hRight
    (declare (salience 90))
    (Turno M)
    (twoTokens H ?player ?f ?c1 ?f ?c2)
    (wouldFall ?f ?c3)
    (Tablero Juego ?f ?c4 ?player)
    (test (or (eq ?player M) (eq ?player J)))

    (test (= (+ ?c1 2) ?c3))
    (test (= (+ ?c1 3) ?c4))
        =>
    (assert (canWin ?player ?f ?c3))
)

;   Si hay dos fichas, un espacio y otra ficha: también puede ganar.
(defrule canWin_separated_hLeft
    (declare (salience 90))
    (Turno M)

    (Tablero Juego ?f ?c1 ?player)
    (wouldFall ?f ?c2)
    (twoTokens H ?player ?f ?c3 ?f ?c4)

    (test (or (eq ?player M) (eq ?player J)))

    (test (= (+ ?c1 1) ?c2))
    (test (= (+ ?c1 2) ?c3))
        =>
    (assert (canWin ?player ?f ?c2))
)

;   Diagonales por cada extremo.
(defrule canWin_mainDiagRight
    (declare (salience 90))
    (Turno M)
    (threeTokensInLine D1 ?player ?f1 ?c1 ?f3 ?c3)
    (wouldFall ?f4 ?c4)
    (test (= (+ ?c3 1) ?c4))
    (test (= (+ ?f3 1) ?f4))
    (test (or (eq ?player M) (eq ?player J)))
        =>
    (assert (canWin ?player ?c4))
)

(defrule canWin_mainDiagLeft
    (declare (salience 90))
    (Turno M)
    (threeTokensInLine D1 ?player ?f1 ?c1 ?f3 ?c3)
    (wouldFall ?f4 ?c4)
    (test (= (- ?c1 1) ?c4))
    (test (= (- ?f1 1) ?f4))
    (test (or (eq ?player M) (eq ?player J)))
        =>
    (assert (canWin ?player ?c4))
)

(defrule canWin_invDiagRight
    (declare (salience 90))
    (Turno M)
    (threeTokensInLine D2 ?player ?f1 ?c1 ?f3 ?c3)
    (wouldFall ?f4 ?c4)
    (test (= (+ ?c1 1) ?c4))
    (test (= (- ?f1 1) ?f4))
    (test (or (eq ?player M) (eq ?player J)))
        =>
    (assert (canWin ?player ?c4))
)

(defrule canWin_invDiagLeft
    (declare (salience 90))
    (Turno M)
    (threeTokensInLine D2 ?player ?f1 ?c1 ?f3 ?c3)
    (wouldFall ?f4 ?c4)
    (test (= (- ?c3 1) ?c4))
    (test (= (+ ?f3 1) ?f4))
    (test (or (eq ?player M) (eq ?player J)))
        =>
    (assert (canWin ?player ?c4))
)


; REGLAS BASADAS EN MI CONOCIMIENTO

; Mi Conocimiento:
; - Nunca he jugado a este juego antes, por lo tanto mi estrategia personal es ir estar a la defensiva e intentar evitar que
;   mi contrincante avance mientras que tambien dentro de lo posible intento tambien ir haciendo yo un 4 en raya.

; Reglas Defensivas:
; - Si detecta que el contrincante ha colocado dos piezas juntas y tiene un hueco de un lado, insertar una pieza.

;   Si hay dos fichas del jugador en horizontal y en la siguiente columna puedo meter una ficha, lo hago.
(defrule defend_h_r
    (declare (salience 30))
    ?turn <- (Turno M)
    ; Si hay dos fichas horizontales en la fila f
    (twoTokens H J ?f ?c1 ?f ?c2)
    ; Y caería una ficha en la fila f, columna c3
    ?rule <- (wouldFall ?f ?c3)
    ; Si c3=c2+1 entonces hay hueco por la derehca, puedo colocarla.
    (test (= (+ ?c2 1) ?c3))
       =>
    (printout t "Juego de manera defensiva horizontal derecha en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

;   Idem por el otro lado.
(defrule defend_h_l
    (declare (salience 30))
    ?turn <- (Turno M)
    (twoTokens H J ?f ?c1 ?f ?c2)
    ?rule <- (wouldFall ?f ?c3)
    (test (= (- ?c1 1) ?c3))
        =>
    (printout t "Juego de manera defensiva horizontal izquierda en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

;   Si hay dos fichas puestas verticalmente y puedo colocar yo una ficha encima, lo hago.
(defrule defend_v
    (declare (salience 30))
    ?turn <- (Turno M)
    (twoTokens V J ?f1 ?c ?f ?c)
    ?rule <- (wouldFall ?f3 ?c)
    (test (= (- ?f1 1) ?f3))
        =>
    (printout t "Juego de manera defensiva vertical en la columna " ?c crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c))
)

;   Si hay dos fichas en diagonal y puedo colocar una tercera para trancar la jugada, lo hago.
(defrule defend_d1_top
    (declare (salience 25))
    ?turn <- (Turno M)
    (twoTokens D1 J ?f1 ?c1 ?f2 ?c2)
    ?rule <- (wouldFall ?f3 ?c3)
    (test (= (- ?f1 1) ?f3))
    (test (= (- ?c1 1) ?c3))
        =>
    (printout t "Juego de manera defensiva diagonal principal arriba en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

;   Idem.
(defrule defend_d1_bottom
    (declare (salience 25))
    ?turn <- (Turno M)
    (twoTokens D1 J ?f1 ?c1 ?f2 ?c2)
    ?rule <- (wouldFall ?f3 ?c3)
    (test (= (+ ?f2 1) ?f3))
    (test (= (+ ?c2 1) ?c3))
        =>
    (printout t "Juego de manera defensiva por diagonal principal abajo en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

;   Idem.
(defrule defend_d2_top
    (declare (salience 25))
    ?turn <- (Turno M)
    (twoTokens D2 J ?f1 ?c1 ?f2 ?c2)
    ?rule <- (wouldFall ?f3 ?c3)
    (test (= (- ?f2 1) ?f3))
    (test (= (+ ?c2 1) ?c3))
        =>
    (printout t "Juego de manera defensiva por diagonal inversa arriba en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

;   Idem.
(defrule defend_d2_bottom
    (declare (salience 25))
    ?turn <- (Turno M)
    (twoTokens D2 J ?f1 ?c1 ?f2 ?c2)
    ?rule <- (wouldFall ?f3 ?c3)
    (test (= (+ ?f1 1) ?f3))
    (test (= (- ?c1 1) ?c3))
        =>
    (printout t "Juego de manera defensiva por diagonal inversa abajo en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)


; Reglas Ofensivas:
; - Si no hay movimientos que pongan en peligro la partida, intentar realizar un 4 en raya.

;   Movimientos con una ficha ya en el tablero.
(defrule make1stMove_hRight
    (declare (salience 10))
    ?turn <- (Turno M)
    ; La posición siguiente (derecha) a una ficha que ha puesto la máquina.
    (nextPos H ?f ?c M)
    ; Y es una posición válida...
    (wouldFall ?f ?c)
    =>
    (printout t "Juego de manera ofensiva horizontal por la derecha en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

;   Idem por la izquierda.
(defrule make1stMove_hLeft
    (declare (salience 10))
    ?turn <- (Turno M)
    (prevPos H ?f ?c M)
    (wouldFall ?f ?c)
    =>
    (printout t "Juego de manera ofensiva horizontal por la izquierda en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

;   Idem
(defrule make1stMove_vertical
    (declare (salience 10))
    ?turn <- (Turno M)
    (prevPos V ?f ?c M)
    (wouldFall ?f ?c)
    =>
    (printout t "Juego de manera ofensiva vertical en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)


(defrule make1stMove_mainDiagRight
    (declare (salience 10))
    ?turn <- (Turno M)
    (nextPos D1 ?f ?c M)
    (wouldFall ?f ?c)
    =>
    (printout t "Juego de manera ofensiva diagonal en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

(defrule make1stMove_mainDiagLeft
    (declare (salience 10))
    ?turn <- (Turno M)
    (prevPos D1 ?f ?c M)
    (wouldFall ?f ?c)
    =>
    (printout t "Juego de manera ofensiva diagonal en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

(defrule make1stMove_invDiagRight
    (declare (salience 10))
    ?turn <- (Turno M)
    (nextPos D2 ?f ?c M)
    (wouldFall ?f ?c)
    =>
    (printout t "Juego de manera ofensiva diagonal en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

(defrule make1stMove_invDiagLeft
    (declare (salience 10))
    ?turn <- (Turno M)
    (prevPos D2 ?f ?c M)
    (wouldFall ?f ?c)
    =>
    (printout t "Juego de manera ofensiva diagonal en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)


;       Movimientos con dos fichas seguidas en el tablero
(defrule make2ndMove_hLeft
    (declare (salience 20))
    ?turn <- (Turno M)
    (twoTokens H M ?f ?c1 ?f ?c2)
    (wouldFall ?f ?c)
    (test (= (+ ?c 1) ?c1))
    =>
    (printout t "Juego de manera ofensiva horizontal izquierda en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

(defrule make2ndMove_hRight
    (declare (salience 20))
    ?turn <- (Turno M)
    (twoTokens H M ?f ?c1 ?f ?c2)
    (wouldFall ?f ?c)
    (test (= (+ ?c2 1) ?c))
    =>
    (printout t "Juego de manera ofensiva horizontal derecha en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

(defrule make2ndMove_vertical
    (declare (salience 20))
    ?turn <- (Turno M)
    (twoTokens V M ?f1 ?c ?f2 ?c)
    (wouldFall ?f3 ?c)
    (test (= (+ ?f2 1) ?f3))
    =>
    (printout t "Juego de manera ofensiva vertical en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

(defrule make2ndMove_d1_top
    (declare (salience 20))
    ?turn <- (Turno M)
    (twoTokens D1 M ?f1 ?c1 ?f2 ?c2)
    ?rule <- (wouldFall ?f3 ?c3)
    (test (= (- ?f1 1) ?f3))
    (test (= (- ?c1 1) ?c3))
        =>
    (printout t "Juego de manera ofensiva diagonal principal arriba en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

(defrule make2ndMove_d1_bottom
    (declare (salience 20))
    ?turn <- (Turno M)
    (twoTokens D1 M ?f1 ?c1 ?f2 ?c2)
    ?rule <- (wouldFall ?f3 ?c3)
    (test (= (+ ?f2 1) ?f3))
    (test (= (+ ?c2 1) ?c3))
        =>
    (printout t "Juego de manera ofensiva por diagonal principal abajo en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

(defrule make2ndMove_d2_top
    (declare (salience 20))
    ?turn <- (Turno M)
    (twoTokens D2 M ?f1 ?c1 ?f2 ?c2)
    ?rule <- (wouldFall ?f3 ?c3)
    (test (= (- ?f2 1) ?f3))
    (test (= (+ ?c2 1) ?c3))
        =>
    (printout t "Juego de manera ofensiva por diagonal inversa arriba en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

(defrule make2ndMove_d2_bottom
    (declare (salience 20))
    ?turn <- (Turno M)
    (twoTokens D2 M ?f1 ?c1 ?f2 ?c2)
    ?rule <- (wouldFall ?f3 ?c3)
    (test (= (+ ?f1 1) ?f3))
    (test (= (- ?c1 1) ?c3))
        =>
    (printout t "Juego de manera ofensiva por diagonal inversa abajo en la columna " ?c3 crlf)
    (retract ?turn)
    (retract ?rule)
    (assert (Juega M ?c3))
)

;       Si el bot detecta que puede realizar una jugada ganadora, tiene prioridad sobre el resto de reglas.
(defrule tryToWin
    (declare (salience 85))
    ?turn <- (Turno M)
    (canWin M ?c)
        =>
    (printout t "Juego de manera ofensiva para ganar en la columna " ?c crlf)
    (retract ?turn)
    (assert (Juega M ?c))
)

;       Limpiar las reglas que indican donde caeria una pieza ya que esto varia de turno a turno.
(defrule cleanUp_WouldFall
    (declare (salience 5))
    (Juega M ?)
    ?rule <- (wouldFall ?f ?c)
        =>
    (retract ?rule)
)