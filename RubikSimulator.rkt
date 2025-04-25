#lang racket
(require racket/draw racket/gui/base)

;; Función para obtener una cara del cubo por su nombre/posición
(define (obtener-cara cubo nombre)
  (cond
    ((null? cubo) '())
    ((equal? (caar cubo) nombre) (car cubo))
    (else (obtener-cara (cdr cubo) nombre))))

;; Función directa para obtener el contenido de una cara
(define (obtener-contenido-cara cubo nombre)
  (cadr (obtener-cara cubo nombre)))

;; Función para actualizar una cara en el cubo
(define (actualizar-cara cubo nombre nuevo-contenido)
  (cond
    ((null? cubo) '())
    ((equal? (caar cubo) nombre) 
     (cons (list nombre nuevo-contenido) (cdr cubo)))
    (else (cons (car cubo) (actualizar-cara (cdr cubo) nombre nuevo-contenido)))))

;; FUNCIONES PARA MANIPULAR ELEMENTOS DE LAS CARAS

;; Función para obtener un elemento específico de una cara por índice
(define (obtener-elemento-por-indice cara indice)
  (cond
    ((= indice 0) (car cara))
    (else (obtener-elemento-por-indice (cdr cara) (- indice 1)))))

;; Función para actualizar un elemento específico de una cara por índice
(define (actualizar-elemento-por-indice cara indice nuevo-valor)
  (cond
    ((null? cara) '())
    ((= indice 0) (cons nuevo-valor (cdr cara)))
    (else (cons (car cara) (actualizar-elemento-por-indice (cdr cara) (- indice 1) nuevo-valor)))))

;; Función para convertir posición fila/columna a índice lineal
(define (posicion-a-indice fila columna tamaño)
  (+ (- columna 1) (* (- fila 1) tamaño)))

;; Función para obtener un elemento específico de una cara
(define (obtener-elemento cara fila columna tamaño)
  (obtener-elemento-por-indice cara (+ (- columna 1) (* (- fila 1) tamaño))))

;; Función para actualizar un elemento específico de una cara
(define (actualizar-elemento cara fila columna nuevo-valor tamaño)
  (actualizar-elemento-por-indice cara (+ (- columna 1) (* (- fila 1) tamaño)) nuevo-valor))

;; FUNCIONES PARA MANIPULAR FILAS Y COLUMNAS

;; Función para obtener una fila completa de una cara
(define (obtener-fila cara fila-num tamaño)
  (define (extraer-fila cara indice-base col max-col resultado)
    (cond
      ((> col max-col) resultado)
      (else (extraer-fila cara indice-base (+ col 1) max-col 
                        (append resultado (list (obtener-elemento-por-indice cara (+ indice-base (- col 1))))))))) 
  (extraer-fila cara (* (- fila-num 1) tamaño) 1 tamaño '()))

;; Función para obtener una columna completa de una cara
(define (obtener-columna cara columna-num tamaño)
  (define (extraer-columna cara col fila max-fila resultado)
    (cond
      ((> fila max-fila) resultado)
      (else (extraer-columna cara col (+ fila 1) max-fila 
                           (append resultado (list (obtener-elemento cara fila col tamaño))))))) 
  (extraer-columna cara columna-num 1 tamaño '()))

;; Función para actualizar una fila completa de una cara
(define (actualizar-fila cara fila-num nueva-fila tamaño)
  (define (actualizar-elementos-fila cara indice-base col max-col nueva-fila resultado)
    (cond
      ((> col max-col) resultado)
      (else (actualizar-elementos-fila 
             cara indice-base (+ col 1) max-col (cdr nueva-fila)
             (actualizar-elemento-por-indice resultado (+ indice-base (- col 1)) (car nueva-fila))))))
  (actualizar-elementos-fila cara (* (- fila-num 1) tamaño) 1 tamaño nueva-fila cara))

;; Función para actualizar una columna completa de una cara
(define (actualizar-columna cara columna-num nueva-columna tamaño)
  (define (actualizar-elementos-columna cara fila col tamaño nueva-columna resultado)
    (cond
      ((> fila tamaño) resultado)
      (else (actualizar-elementos-columna 
             cara (+ fila 1) col tamaño (cdr nueva-columna)
             (actualizar-elemento resultado fila col (car nueva-columna) tamaño)))))
  (actualizar-elementos-columna cara 1 columna-num tamaño nueva-columna cara))

;; FUNCIONES PARA ROTAR CARAS

;; Función para rotar una cara en sentido horario
(define (rotar-cara-horario cara tamaño)
  (define (construir-cara-rotada fila col tamaño origen resultado)
    (cond
      ((> fila tamaño) resultado)
      ((> col tamaño) (construir-cara-rotada (+ fila 1) 1 tamaño origen resultado))
      (else (construir-cara-rotada fila (+ col 1) tamaño origen 
                          (actualizar-elemento resultado (- (+ tamaño 1) col) fila 
                                               (obtener-elemento origen fila col tamaño) tamaño)))))
  (construir-cara-rotada 1 1 tamaño cara cara))

;; Función para rotar una cara en sentido antihorario
(define (rotar-cara-antihorario cara tamaño)
  (define (construir-cara-rotada fila col tamaño origen resultado)
    (cond
      ((> fila tamaño) resultado)
      ((> col tamaño) (construir-cara-rotada (+ fila 1) 1 tamaño origen resultado))
      (else (construir-cara-rotada fila (+ col 1) tamaño origen 
                          (actualizar-elemento resultado col (- (+ tamaño 1) fila) 
                                               (obtener-elemento origen fila col tamaño) tamaño)))))
  (construir-cara-rotada 1 1 tamaño cara cara))

;; MOVIMIENTOS DEL CUBO

;; Función para mover una fila hacia la derecha
(define (mover-fila-derecha-car-cdr cubo fila tamaño)
  (list 
   (list "Cara blanca" 
         (actualizar-fila 
          (cadr (obtener-cara cubo "Cara blanca")) 
          fila 
          (obtener-fila (cadr (obtener-cara cubo "Cara azul")) fila tamaño) 
          tamaño))
   (list "Cara verde" 
         (actualizar-fila 
          (cadr (obtener-cara cubo "Cara verde")) 
          fila 
          (obtener-fila (cadr (obtener-cara cubo "Cara blanca")) fila tamaño) 
          tamaño))
   (list "Cara amarilla" 
         (actualizar-fila 
          (cadr (obtener-cara cubo "Cara amarilla")) 
          fila 
          (obtener-fila (cadr (obtener-cara cubo "Cara verde")) fila tamaño) 
          tamaño))
   (list "Cara azul" 
         (actualizar-fila 
          (cadr (obtener-cara cubo "Cara azul")) 
          fila 
          (obtener-fila (cadr (obtener-cara cubo "Cara amarilla")) fila tamaño) 
          tamaño))
   (list "Cara roja" 
         (if (= fila 1)
             (rotar-cara-horario (cadr (obtener-cara cubo "Cara roja")) tamaño)
             (cadr (obtener-cara cubo "Cara roja"))))
   (list "Cara naranja" 
         (if (= fila tamaño)
             (rotar-cara-antihorario (cadr (obtener-cara cubo "Cara naranja")) tamaño)
             (cadr (obtener-cara cubo "Cara naranja"))))))

;; Función para mover una fila hacia la izquierda
(define (mover-fila-izquierda-car-cdr cubo fila tamaño)
  (list 
   (list "Cara blanca" 
         (actualizar-fila 
          (cadr (obtener-cara cubo "Cara blanca")) 
          fila 
          (obtener-fila (cadr (obtener-cara cubo "Cara verde")) fila tamaño) 
          tamaño))
   (list "Cara verde" 
         (actualizar-fila 
          (cadr (obtener-cara cubo "Cara verde")) 
          fila 
          (obtener-fila (cadr (obtener-cara cubo "Cara amarilla")) fila tamaño) 
          tamaño))
   (list "Cara amarilla" 
         (actualizar-fila 
          (cadr (obtener-cara cubo "Cara amarilla")) 
          fila 
          (obtener-fila (cadr (obtener-cara cubo "Cara azul")) fila tamaño) 
          tamaño))
   (list "Cara azul" 
         (actualizar-fila 
          (cadr (obtener-cara cubo "Cara azul")) 
          fila 
          (obtener-fila (cadr (obtener-cara cubo "Cara blanca")) fila tamaño) 
          tamaño))
   (list "Cara roja" 
         (if (= fila 1)
             (rotar-cara-antihorario (cadr (obtener-cara cubo "Cara roja")) tamaño)
             (cadr (obtener-cara cubo "Cara roja"))))
   (list "Cara naranja" 
         (if (= fila tamaño)
             (rotar-cara-horario (cadr (obtener-cara cubo "Cara naranja")) tamaño)
             (cadr (obtener-cara cubo "Cara naranja"))))))

;; Función para mover una columna hacia arriba
(define (mover-columna-arriba-car-cdr cubo columna tamaño)
  (list 
   (list "Cara blanca" 
         (actualizar-columna 
          (cadr (obtener-cara cubo "Cara blanca")) 
          columna 
          (obtener-columna (cadr (obtener-cara cubo "Cara naranja")) columna tamaño) 
          tamaño))
   (list "Cara verde" 
         (if (= columna tamaño)
             (rotar-cara-horario (cadr (obtener-cara cubo "Cara verde")) tamaño)
             (cadr (obtener-cara cubo "Cara verde"))))
   (list "Cara amarilla" 
         (actualizar-columna 
          (cadr (obtener-cara cubo "Cara amarilla")) 
          (- (+ tamaño 1) columna) 
          (reverse (obtener-columna (cadr (obtener-cara cubo "Cara roja")) columna tamaño)) 
          tamaño))
   (list "Cara azul" 
         (if (= columna 1)
             (rotar-cara-antihorario (cadr (obtener-cara cubo "Cara azul")) tamaño)
             (cadr (obtener-cara cubo "Cara azul"))))
   (list "Cara roja" 
         (actualizar-columna 
          (cadr (obtener-cara cubo "Cara roja")) 
          columna 
          (obtener-columna (cadr (obtener-cara cubo "Cara blanca")) columna tamaño) 
          tamaño))
   (list "Cara naranja" 
         (actualizar-columna 
          (cadr (obtener-cara cubo "Cara naranja")) 
          columna 
          (reverse (obtener-columna (cadr (obtener-cara cubo "Cara amarilla")) (- (+ tamaño 1) columna) tamaño)) 
          tamaño))))

;; Función para mover una columna hacia abajo
(define (mover-columna-abajo-car-cdr cubo columna tamaño)
  (list 
   (list "Cara blanca" 
         (actualizar-columna 
          (cadr (obtener-cara cubo "Cara blanca")) 
          columna 
          (obtener-columna (cadr (obtener-cara cubo "Cara roja")) columna tamaño) 
          tamaño))
   (list "Cara verde" 
         (if (= columna tamaño)
             (rotar-cara-antihorario (cadr (obtener-cara cubo "Cara verde")) tamaño)
             (cadr (obtener-cara cubo "Cara verde"))))
   (list "Cara amarilla" 
         (actualizar-columna 
          (cadr (obtener-cara cubo "Cara amarilla")) 
          (- (+ tamaño 1) columna) 
          (reverse (obtener-columna (cadr (obtener-cara cubo "Cara naranja")) columna tamaño)) 
          tamaño))
   (list "Cara azul" 
         (if (= columna 1)
             (rotar-cara-horario (cadr (obtener-cara cubo "Cara azul")) tamaño)
             (cadr (obtener-cara cubo "Cara azul"))))
   (list "Cara roja" 
         (actualizar-columna 
          (cadr (obtener-cara cubo "Cara roja")) 
          columna 
          (reverse (obtener-columna (cadr (obtener-cara cubo "Cara amarilla")) (- (+ tamaño 1) columna) tamaño)) 
          tamaño))
   (list "Cara naranja" 
         (actualizar-columna 
          (cadr (obtener-cara cubo "Cara naranja")) 
          columna 
          (obtener-columna (cadr (obtener-cara cubo "Cara blanca")) columna tamaño) 
          tamaño))))

;; GRAFOS PARA CADA TAMAÑO DE CUBO.

(define (crear-cubo-2x2)
  (list
   (list "Cara blanca" (list 'WRB 'WRG 'WOB 'WOG))
   (list "Cara verde" (list 'GRW 'GRY 'GOW 'GOY))
   (list "Cara amarilla" (list 'YRG 'YRB 'YOG 'YOB))
   (list "Cara azul" (list 'BRY 'BRW 'BOY 'BOW))
   (list "Cara roja" (list 'RYB 'RYG 'RWB 'RWG))
   (list "Cara naranja" (list 'OWB 'OWY 'OYB 'OYG))))

(define (crear-cubo-3x3)
  (list
   (list "Cara blanca" (list 'WRB 'WR 'WRG 'WB 'W 'WG 'WOB 'WO 'WOG))
   (list "Cara verde" (list 'GRW 'GR 'GRY 'GW 'G 'GY 'GOW 'GO 'GOY))
   (list "Cara amarilla" (list 'YRG 'YR 'YRB 'YG 'Y 'YB 'YOG 'YO 'YOB))
   (list "Cara azul" (list 'BRY 'BR 'BRW 'BY 'B 'BW 'BOY 'BO 'BOW))
   (list "Cara roja" (list 'RYB 'RY 'RYG 'RB 'R 'RG 'RWB 'RW 'RWG))
   (list "Cara naranja" (list 'OWB 'OW 'OWG 'OB 'O 'OG 'OYB 'OY 'OYG))))

(define (crear-cubo-4x4)
  (list
   (list "Cara blanca" (list 'WRB 'WR1 'WR2 'WRG 'WB1 'W1 'W2 'WG1 'WB2 'W3 'W4 'WG2 'WOB 'WO1 'WO2 'WOG))
   (list "Cara verde" (list 'GRW 'GR1 'GR2 'GRY 'GW1 'G1 'G2 'GY1 'GW2 'G3 'G4 'GY2 'GOW 'GO1 'GO2 'GOY))
   (list "Cara amarilla" (list 'YRG 'YR1 'YR2 'YRB 'YG1 'Y1 'Y2 'YB1 'YG2 'Y3 'Y4 'YB2 'YOG 'YO1 'YO2 'YOB))
   (list "Cara azul" (list 'BRY 'BR1 'BR2 'BRW 'BY1 'B1 'B2 'BW1 'BY2 'B3 'B4 'BW2 'BOY 'BO1 'BO2 'BOW))
   (list "Cara roja" (list 'RYB 'RY1 'RY2 'RYG 'RB1 'R1 'R2 'RG1 'RB2 'R3 'R4 'RG2 'RWB 'RW1 'RW2 'RWG))
   (list "Cara naranja" (list 'OWB 'OW1 'OW2 'OWG 'OB1 'O1 'O2 'OG1 'OB2 'O3 'O4 'OG2 'OYB 'OY1 'OY2 'OYG))))

(define (crear-cubo-5x5)
  (list
   (list "Cara blanca" (list 'WRB 'WR1 'WR2 'WR3 'WRG 'WB1 'W1 'W2 'W3 'WG1 'WB2 'W4 'W5 'W6 'WG2 'WB3 'W7 'W8 'W9 'WG3 'WOB 'WO1 'WO2 'WO3 'WOG))
   (list "Cara verde" (list 'GRW 'GR1 'GR2 'GR3 'GRY 'GW1 'G1 'G2 'G3 'GY1 'GW2 'G4 'G5 'G6 'GY2 'GW3 'G7 'G8 'G9 'GY3 'GOY 'GO1 'GO2 'GO3 'GOY))
   (list "Cara amarilla" (list 'YRG 'YR1 'YR2 'YR3 'YRB 'YG1 'Y1 'Y2 'Y3 'YB1 'YG2 'Y4 'Y5 'Y6 'YB2 'YG3 'Y7 'Y8 'Y9 'YB3 'YOG 'YO1 'YO2 'YO3 'YBO))
   (list "Cara azul" (list 'BRY 'BR1 'BR2 'BR3 'BRW 'BY1 'B1 'B2 'B3 'BW1 'BY2 'B4 'B5 'B6 'BW2 'BY3 'B7 'B8 'B9 'BW3 'BOY 'BO1 'BO2 'BO3 'BOW))
   (list "Cara roja" (list 'RYB 'RY1 'RY2 'RY3 'RYG 'RB1 'R1 'R2 'R3 'RG1 'RB2 'R4 'R5 'R6 'RG2 'RB3 'R7 'R8 'R9 'RG3 'RWB 'RW1 'RW2 'RW3 'RWG))
   (list "Cara naranja" (list 'OWB 'OW1 'OW2 'OW3 'OWG 'OB1 'O1 'O2 'O3 'OG1 'OB2 'O4 'O5 'O6 'OG2 'OB3 'O7 'O8 'O9 'OG3 'OYB 'OY1 'OY2 'OY3 'OYG))))

(define (crear-cubo-6x6)
  (list
   (list "Cara blanca" (list 'WRB 'WR1 'WR2 'WR3 'WR4 'WRG 'WB1 'W1 'W2 'W3 'W4 'WG1 'WB2 'W5 'W6 'W7 'W8 'WG2 'WB3 'W9 'W10 'W11 'W12 'WG3 'WB4 'W13 'W14 'W15 'W16 'WG4 'WOB 'WO1 'WO2 'WO3 'WO4 'WOG))
   (list "Cara verde" (list 'GRW 'GR1 'GR2 'GR3 'GR4 'GRY 'GW1 'G1 'G2 'G3 'G4 'GY1 'GW2 'G5 'G6 'G7 'G8 'GY2 'GW3 'G9 'G10 'G11 'G12 'GY3 'GW4 'G13 'G14 'G15 'G16 'GY4 'GOW 'GO1 'GO2 'GO3 'GO4 'GOY))
   (list "Cara amarilla" (list 'YRG 'YR1 'YR2 'YR3 'YR4 'YRB 'YG1 'Y1 'Y2 'Y3 'Y4 'YB1 'YG2 'Y5 'Y6 'Y7 'Y8 'YB2 'YG3 'Y9 'Y10 'Y11 'Y12 'YB3 'YG4 'Y13 'Y14 'Y15 'Y16 'YB4 'YOG 'YO1 'YO2 'YO3 'YO4 'YOB))
   (list "Cara azul" (list 'BRY 'BR1 'BR2 'BR3 'BR4 'BRW 'BY1 'B1 'B2 'B3 'B4 'BW1 'BY2 'B5 'B6 'B7 'B8 'BW2 'BY3 'B9 'B10 'B11 'B12 'BW3 'BY4 'B13 'B14 'B15 'B16 'BW4 'BOY 'BO1 'BO2 'BO3 'BO4 'BOW))
   (list "Cara roja" (list 'RYB 'RY1 'RY2 'RY3 'RY4 'RYG 'RB1 'R1 'R2 'R3 'R4 'RG1 'RB2 'R5 'R6 'R7 'R8 'RG2 'RB3 'R9 'R10 'R11 'R12 'RG3 'RB4 'R13 'R14 'R15 'R16 'RG4 'RWB 'RW1 'RW2 'RW3 'R4 'RWG))
   (list "Cara naranja" (list 'OWB 'OW1 'OW2 'OW3 'OW4 'OWG 'OB1 'O1 'O2 'O3 'O4 'OG1 'OB2 'O5 'O6 'O7 'O8 'OG2 'OB3 'O9 'O10 'O11 'O12 'OG3 'OB4 'O13 'O14 'O15 'O16 'OG4 'OYB 'OY1 'OY2 'OY3 'OY4 'OYG))))

;; Función para inicializar el cubo según el tamaño
(define (inicializar-cubo tamaño)
  (cond
    [(= tamaño 2) (crear-cubo-2x2)]
    [(= tamaño 3) (crear-cubo-3x3)]
    [(= tamaño 4) (crear-cubo-4x4)]
    [(= tamaño 5) (crear-cubo-5x5)]
    [(= tamaño 6) (crear-cubo-6x6)]))
;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; FUNCIONES PARA VISUALIZACIÓN DEL CUBO

;; Función para mostrar una cara del cubo
(define (mostrar-cara cara nombre tamaño)
  (displayln (string-append "Cara " nombre ":"))
  (define (mostrar-fila fila-inicio col-total fila-actual col-actual)
    (cond
      ((> col-actual col-total) 
       (newline)
       (if (>= fila-actual col-total)
           (void)
           (mostrar-fila (+ fila-inicio col-total) col-total (+ fila-actual 1) 1)))
      (else
       (display (obtener-elemento-por-indice cara (+ fila-inicio (- col-actual 1))))
       (display " ")
       (mostrar-fila fila-inicio col-total fila-actual (+ col-actual 1)))))
  (mostrar-fila 0 tamaño 1 1))

;; Función para mostrar todo el cubo
(define (mostrar-cubo cubo tamaño)
  (displayln "Estado actual del cubo:")
  (mostrar-cara (cadr (obtener-cara cubo "Cara blanca")) "Frontal (Blanca)" tamaño)
  (mostrar-cara (cadr (obtener-cara cubo "Cara verde")) "Derecha (Verde)" tamaño)
  (mostrar-cara (cadr (obtener-cara cubo "Cara amarilla")) "Trasera (Amarilla)" tamaño)
  (mostrar-cara (cadr (obtener-cara cubo "Cara azul")) "Izquierda (Azul)" tamaño)
  (mostrar-cara (cadr (obtener-cara cubo "Cara roja")) "Superior (Roja)" tamaño)
  (mostrar-cara (cadr (obtener-cara cubo "Cara naranja")) "Inferior (Naranja)" tamaño))

;; INTERFAZ DE USUARIO Y FUNCIONES PRINCIPALES

;; Función para interpretar un movimiento individual
(define (interpretar-movimiento cubo movimiento tamaño)
  (define tipo-movimiento (string-ref (symbol->string movimiento) 0))
  (define numero-fila-columna (string->number (string (string-ref (symbol->string movimiento) 1))))
  (define direccion (string-ref (symbol->string movimiento) 2))
  
  (cond
    ((equal? tipo-movimiento #\F)
     (cond
       ((equal? direccion #\D) (mover-fila-derecha-car-cdr cubo numero-fila-columna tamaño))
       ((equal? direccion #\I) (mover-fila-izquierda-car-cdr cubo numero-fila-columna tamaño))
       (else cubo)))
    ((equal? tipo-movimiento #\C)
     (cond
       ((equal? direccion #\A) (mover-columna-arriba-car-cdr cubo numero-fila-columna tamaño))
       ((equal? direccion #\B) (mover-columna-abajo-car-cdr cubo numero-fila-columna tamaño))
       (else cubo)))
    (else cubo)))

;; Función para aplicar una secuencia de movimientos
(define (aplicar-movimientos cubo movimientos tamaño)
  (cond
    ((null? movimientos) cubo)
    (else (aplicar-movimientos 
           (interpretar-movimiento cubo (car movimientos) tamaño)
           (cdr movimientos) 
           tamaño))))

;; FUNCIONES PARA VISUALIZACIÓN GRÁFICA DEL CUBO
;;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; Función para determinar el color basado en el símbolo
(define (color-de-celda simbolo)
  (define simbolo-str (symbol->string simbolo))
  (define primer-caracter (string-ref simbolo-str 0))
  (cond
    ((equal? primer-caracter #\W) "white")
    ((equal? primer-caracter #\G) "green")
    ((equal? primer-caracter #\Y) "yellow")
    ((equal? primer-caracter #\B) "blue")
    ((equal? primer-caracter #\R) "red")
    ((equal? primer-caracter #\O) "orange")
    (else "gray")))

;; Función para obtener tamaño de cubo basado en contenido
(define (obtener-tamaño-cubo contenido)
  (define longitud (length contenido))
  (exact-floor (sqrt longitud)))

;; Dibuja una cara individual en un panel canvas
(define (dibujar-cara canvas dc contenido x y tamaño)
  (define cell-size 40)
  (define fila 0)
  (define i 0)
  (define j 0)
  (define (loop contenido fila i j)
    (cond
      ((null? contenido) '())
      (else
       (send dc set-brush (make-object color% (color-de-celda (car contenido))) 'solid)
       (send dc draw-rectangle (+ x (* j cell-size)) (+ y (* i cell-size)) cell-size cell-size)
       (send dc set-text-foreground "black")
       (send dc draw-text (symbol->string (car contenido)) (+ x (* j cell-size) 2) (+ y (* i cell-size) 8))
       (if (= (+ j 1) tamaño)
           (loop (cdr contenido) (+ fila 1) (+ i 1) 0)
           (loop (cdr contenido) fila i (+ j 1))))))
  (loop contenido fila i j))

;; Función para mostrar las etiquetas de las caras
(define (mostrar-etiqueta-cara dc texto x y)
  (send dc set-text-foreground "black")
  (send dc draw-text texto x y))

;; FUNCIONES PARA VISUALIZACIÓN GRÁFICA CON ANIMACIÓN

;; Variable global para almacenar el estado actual del cubo animado
(define cubo-animado '())

;; Función para animar movimientos con retraso
(define (animar-movimientos frame canvas cubo-inicial movimientos tamaño)
  ;; Asignar el cubo inicial a la variable global
  (set! cubo-animado cubo-inicial)
  
  ;; Función para actualizar el canvas con el cubo actual
  (define (actualizar-canvas)
    (send canvas refresh)
    (sleep/yield 0.01)) ;; pequeña pausa para asegurar que se dibuje

  ;; Función recursiva para aplicar movimientos con retraso
  (define (animar-siguiente resto-movimientos)
    (cond
      ((null? resto-movimientos) 
       (actualizar-canvas))
      (else
       ;; Mostrar el estado actual
       (actualizar-canvas)
       
       ;; Esperar 1 segundo
       (sleep/yield 1)
       
       ;; Aplicar el siguiente movimiento y actualizar variable global
       (set! cubo-animado (interpretar-movimiento 
                           cubo-animado 
                           (car resto-movimientos) 
                           tamaño))
       
       ;; Refrescar el canvas para mostrar el nuevo estado
       (actualizar-canvas)
       
       ;; Continuar con el siguiente movimiento
       (animar-siguiente (cdr resto-movimientos)))))
  
  ;; Iniciar la animación después de configurar todo
  (actualizar-canvas)
  (animar-siguiente movimientos))

;; Función principal modificada para mostrar la interfaz gráfica con animación
(define (mostrar-interfaz-grafica-animada cubo-inicial movimientos tamaño)
  ;; Asegurarse de que cubo-animado esté inicializado desde el principio
  (set! cubo-animado cubo-inicial)
  
  ;; Crear ventana
  (define frame (new frame% (label (format "Cubo de Rubik ~ax~a Animado" tamaño tamaño)) 
                     (width 1200) (height 1000)))

  ;; Canvas donde se dibujarán las caras
  (define canvas
    (new canvas%
         (parent frame)
         (paint-callback
          (lambda (canvas dc)
            ;; Verificación de seguridad para el cubo-animado
            (when (not (null? cubo-animado))
              ;; Posiciones x, y para mostrar las caras en orden de cruz
              (define posiciones
                (cond
                  ((= tamaño 2)
                   '((250 180)     ;; blanca
                     (400 180)    ;; verde
                     (550 180)    ;; amarilla
                     (100 180)    ;; azul
                     (250 50)    ;; roja
                     (250 310)))  ;; naranja
                  ((= tamaño 3)
                   '((250 180)     ;; blanca
                     (400 180)    ;; verde
                     (550 180)    ;; amarilla
                     (100 180)    ;; azul
                     (250 30)    ;; roja
                     (250 330)))  ;; naranja
                  ((= tamaño 4)
                   '((260 200)     ;; blanca
                     (430 200)     ;; verde
                     (600 200)    ;; amarilla
                     (80 200)    ;; azul
                     (260 20)    ;; roja
                     (260 380)))  ;; naranja
                  ((= tamaño 5)
                   '((310 300)     ;; blanca
                     (550 300)     ;; verde
                     (800 300)    ;; amarilla
                     (60 300)    ;; azul
                     (310 30)    ;; roja
                     (310 550)))  ;; naranja
                  ((= tamaño 6)
                   '((310 320)     ;; blanca
                     (600 320)     ;; verde
                     (890 320)    ;; amarilla
                     (20 320)    ;; azul
                     (310 20)    ;; roja
                     (310 660)))))  ;; naranja
                  

              (define etiquetas
                '("Cara Blanca" "Cara Verde" "Cara Amarilla" 
                  "Cara Azul" "Cara Roja" "Cara Naranja"))

              ;; Dibujar etiquetas primero
              (define (dibujar-etiquetas etiquetas posiciones)
                (cond
                  ((null? etiquetas) '())
                  (else
                   (define etiqueta (car etiquetas))
                   (define pos (car posiciones))
                   (mostrar-etiqueta-cara dc etiqueta (car pos) (- (cadr pos) 20))
                   (dibujar-etiquetas (cdr etiquetas) (cdr posiciones)))))

              ;; Dibujar todas las caras del cubo actual
              (define (dibujar-todas posiciones nombres)
                (cond
                  ((null? posiciones) '())
                  (else
                   (define pos (car posiciones))
                   (define nombre (car nombres))
                   ;; Solo dibujar si la cara existe
                   (define cara (obtener-cara cubo-animado nombre))
                   (when (not (null? cara))
                     (dibujar-cara canvas dc 
                                  (cadr cara)
                                  (car pos) (cadr pos) tamaño))
                   (dibujar-todas (cdr posiciones) (cdr nombres)))))

              (dibujar-etiquetas etiquetas posiciones)
              (dibujar-todas posiciones
                            '("Cara blanca" "Cara verde" "Cara amarilla" 
                              "Cara azul" "Cara roja" "Cara naranja")))))))

  ;; Mostrar la ventana
  (send frame show #t)
  
  ;; Iniciar la animación después de un breve retardo para asegurar que la ventana esté dibujada
  (sleep/yield 0.5)
  (animar-movimientos frame canvas cubo-inicial movimientos tamaño))

;; Funcion RS para iniciar el programa
(define (RS tamaño cubo-inicial movimientos)
  (define cubo-base 
    (cond
      ((null? cubo-inicial) (inicializar-cubo tamaño))
      (else (car cubo-inicial))))
  
  ;; Mostrar el cubo en consola
  (mostrar-cubo cubo-base tamaño)
  
  ;; Mostrar la interfaz gráfica animada
  (mostrar-interfaz-grafica-animada cubo-base movimientos tamaño)
  
  ;; Aplicar los movimientos para retornar el estado final (sin animación)
  (aplicar-movimientos cubo-base movimientos tamaño))



;; Ejemplos de uso:
;; (RS 3 '() '(F1D C2A F3I))  ; Inicializa un cubo 3x3 y aplica movimientos
;; (RS 2 '() '(C1B F2I))      ; Inicializa un cubo 2x2 y aplica movimientos
