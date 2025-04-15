#lang racket

;; Función para obtener el elemento en la posición fila/columna específica de una cara
(define (obtener-elemento cara fila columna tamaño)
  (cond
    [(= tamaño 2) 
     (cond
       [(and (= fila 1) (= columna 1)) (car cara)]
       [(and (= fila 1) (= columna 2)) (car (cdr cara))]
       [(and (= fila 2) (= columna 1)) (car (cdr (cdr cara)))]
       [(and (= fila 2) (= columna 2)) (car (cdr (cdr (cdr cara))))])]
    [(= tamaño 3)
     (cond
       [(and (= fila 1) (= columna 1)) (car cara)]
       [(and (= fila 1) (= columna 2)) (car (cdr cara))]
       [(and (= fila 1) (= columna 3)) (car (cdr (cdr cara)))]
       [(and (= fila 2) (= columna 1)) (car (cdr (cdr (cdr cara))))]
       [(and (= fila 2) (= columna 2)) (car (cdr (cdr (cdr (cdr cara)))))]
       [(and (= fila 2) (= columna 3)) (car (cdr (cdr (cdr (cdr (cdr cara))))))]
       [(and (= fila 3) (= columna 1)) (car (cdr (cdr (cdr (cdr (cdr (cdr cara)))))))]
       [(and (= fila 3) (= columna 2)) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr cara))))))))]
       [(and (= fila 3) (= columna 3)) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr cara)))))))))])]
    [(>= tamaño 4)
     (define indice (+ (- columna 1) (* (- fila 1) tamaño)))
     (define (obtener-en-indice lista idx actual)
       (if (= actual idx)
           (car lista)
           (obtener-en-indice (cdr lista) idx (+ actual 1))))
     (obtener-en-indice cara indice 0)]))

;; Función para actualizar un elemento en la posición fila/columna específica de una cara
(define (actualizar-elemento cara fila columna nuevo-valor tamaño)
  (cond
    [(= tamaño 2)
     (cond
       [(and (= fila 1) (= columna 1)) 
        (cons nuevo-valor (cdr cara))]
       [(and (= fila 1) (= columna 2)) 
        (cons (car cara) (cons nuevo-valor (cdr (cdr cara))))]
       [(and (= fila 2) (= columna 1)) 
        (cons (car cara) (cons (car (cdr cara)) (cons nuevo-valor (cdr (cdr (cdr cara))))))]
       [(and (= fila 2) (= columna 2)) 
        (cons (car cara) (cons (car (cdr cara)) (cons (car (cdr (cdr cara))) (cons nuevo-valor '()))))])]
    [(>= tamaño 3)
     (define (actualizar-en-indice lista idx nuevo-val actual)
       (if (= actual idx)
           (cons nuevo-val (cdr lista))
           (cons (car lista) (actualizar-en-indice (cdr lista) idx nuevo-val (+ actual 1)))))
     (define indice (+ (- columna 1) (* (- fila 1) tamaño)))
     (actualizar-en-indice cara indice nuevo-valor 0)]))

;; Función para obtener una fila completa de una cara
(define (obtener-fila cara fila-num tamaño)
  (define (construir-fila cara fila col tamaño resultado)
    (if (> col tamaño)
        resultado
        (construir-fila cara fila (+ col 1) tamaño 
                        (append resultado (cons (obtener-elemento cara fila col tamaño) '())))))
  (construir-fila cara fila-num 1 tamaño '()))

;; Función para obtener una columna completa de una cara
(define (obtener-columna cara columna-num tamaño)
  (define (construir-columna cara fila col tamaño resultado)
    (if (> fila tamaño)
        resultado
        (construir-columna cara (+ fila 1) col tamaño 
                           (append resultado (cons (obtener-elemento cara fila col tamaño) '())))))
  (construir-columna cara 1 columna-num tamaño '()))

;; Función para actualizar una fila completa de una cara
(define (actualizar-fila cara fila-num nueva-fila tamaño)
  (define (actualizar-elementos cara fila col tamaño nueva-fila resultado)
    (if (> col tamaño)
        resultado
        (actualizar-elementos 
         cara fila (+ col 1) tamaño (cdr nueva-fila)
         (actualizar-elemento resultado fila col (car nueva-fila) tamaño))))
  (actualizar-elementos cara fila-num 1 tamaño nueva-fila cara))

;; Función para actualizar una columna completa de una cara
(define (actualizar-columna cara columna-num nueva-columna tamaño)
  (define (actualizar-elementos cara fila col tamaño nueva-columna resultado)
    (if (> fila tamaño)
        resultado
        (actualizar-elementos 
         cara (+ fila 1) col tamaño (cdr nueva-columna)
         (actualizar-elemento resultado fila col (car nueva-columna) tamaño))))
  (actualizar-elementos cara 1 columna-num tamaño nueva-columna cara))

;; Función para rotar una cara en sentido horario
(define (rotar-cara-horario cara tamaño)
  (define (construir-cara fila col tamaño resultado)
    (if (> fila tamaño)
        resultado
        (if (> col tamaño)
            (construir-cara (+ fila 1) 1 tamaño resultado)
            (construir-cara fila (+ col 1) tamaño 
                            (actualizar-elemento resultado col (- tamaño (- fila 1)) 
                                                 (obtener-elemento cara fila col tamaño) tamaño)))))
  (construir-cara 1 1 tamaño cara))

;; Función para rotar una cara en sentido antihorario
(define (rotar-cara-antihorario cara tamaño)
  (define (construir-cara fila col tamaño resultado)
    (if (> fila tamaño)
        resultado
        (if (> col tamaño)
            (construir-cara (+ fila 1) 1 tamaño resultado)
            (construir-cara fila (+ col 1) tamaño 
                            (actualizar-elemento resultado (- tamaño (- col 1)) fila 
                                                 (obtener-elemento cara fila col tamaño) tamaño)))))
  (construir-cara 1 1 tamaño cara))

;; Función para mover una fila hacia la derecha (MODIFICADA)
(define (mover-fila-derecha cubo fila tamaño)
  ;; Extraer caras
  (define frontal (car cubo))
  (define derecha (car (cdr cubo)))
  (define trasera (car (cdr (cdr cubo))))
  (define izquierda (car (cdr (cdr (cdr cubo)))))
  (define superior (car (cdr (cdr (cdr (cdr cubo))))))
  (define inferior (car (cdr (cdr (cdr (cdr (cdr cubo)))))))
  
  ;; Obtener las filas que se van a mover
  (define fila-frontal (obtener-fila frontal fila tamaño))
  (define fila-derecha (obtener-fila derecha fila tamaño))
  (define fila-trasera (obtener-fila trasera fila tamaño))
  (define fila-izquierda (obtener-fila izquierda fila tamaño))
  
  ;; Actualizar caras con las nuevas filas
  (define nueva-cara-frontal (actualizar-fila frontal fila fila-izquierda tamaño))
  (define nueva-cara-derecha (actualizar-fila derecha fila fila-frontal tamaño))
  (define nueva-cara-trasera (actualizar-fila trasera fila fila-derecha tamaño))
  (define nueva-cara-izquierda (actualizar-fila izquierda fila fila-trasera tamaño))
  
  ;; MODIFICACIÓN MÁS PROFUNDA: Para la cara superior específicamente
  (define nueva-cara-superior
    (if (= fila 1)
        ;; Si es la primera fila, invertimos la rotación normal
        (rotar-cara-antihorario superior tamaño) ;; Invertido
        superior)) ;; Si no es la primera fila, no cambia
  
  ;; Para la cara inferior mantenemos la lógica original
  (define nueva-cara-inferior
    (if (= fila tamaño)
        (rotar-cara-horario inferior tamaño)
        inferior))
  
  ;; Construir nuevo cubo con modificación explícita para la cara superior
  (cons nueva-cara-frontal
        (cons nueva-cara-derecha
              (cons nueva-cara-trasera
                    (cons nueva-cara-izquierda
                          (cons nueva-cara-superior
                                (cons nueva-cara-inferior '()))))))
  )

;; Función para mover una fila hacia la izquierda (MODIFICADA)
(define (mover-fila-izquierda cubo fila tamaño)
  ;; Extraer caras
  (define frontal (car cubo))
  (define derecha (car (cdr cubo)))
  (define trasera (car (cdr (cdr cubo))))
  (define izquierda (car (cdr (cdr (cdr cubo)))))
  (define superior (car (cdr (cdr (cdr (cdr cubo))))))
  (define inferior (car (cdr (cdr (cdr (cdr (cdr cubo)))))))
  
  ;; Obtener las filas que se van a mover
  (define fila-frontal (obtener-fila frontal fila tamaño))
  (define fila-derecha (obtener-fila derecha fila tamaño))
  (define fila-trasera (obtener-fila trasera fila tamaño))
  (define fila-izquierda (obtener-fila izquierda fila tamaño))
  
  ;; Actualizar caras con las nuevas filas
  (define nueva-cara-frontal (actualizar-fila frontal fila fila-derecha tamaño))
  (define nueva-cara-derecha (actualizar-fila derecha fila fila-trasera tamaño))
  (define nueva-cara-trasera (actualizar-fila trasera fila fila-izquierda tamaño))
  (define nueva-cara-izquierda (actualizar-fila izquierda fila fila-frontal tamaño))
  
  ;; MODIFICACIÓN MÁS PROFUNDA: Para la cara superior específicamente
  (define nueva-cara-superior
    (if (= fila 1)
        ;; Si es la primera fila, invertimos la rotación normal
        (rotar-cara-antihorario superior tamaño) ;; Invertido
        superior)) ;; Si no es la primera fila, no cambia
  
  ;; Para la cara inferior mantenemos la lógica original
  (define nueva-cara-inferior
    (if (= fila tamaño)
        (rotar-cara-antihorario inferior tamaño)
        inferior))
  
  ;; Construir nuevo cubo con modificación explícita para la cara superior
  (cons nueva-cara-frontal
        (cons nueva-cara-derecha
              (cons nueva-cara-trasera
                    (cons nueva-cara-izquierda
                          (cons nueva-cara-superior
                                (cons nueva-cara-inferior '()))))))
  )

;; Función para mover una columna hacia arriba - MODIFICADA
(define (mover-columna-arriba cubo columna tamaño)
  ;; Extraer caras
  (define frontal (car cubo))
  (define derecha (car (cdr cubo)))
  (define trasera (car (cdr (cdr cubo))))
  (define izquierda (car (cdr (cdr (cdr cubo)))))
  (define superior (car (cdr (cdr (cdr (cdr cubo))))))
  (define inferior (car (cdr (cdr (cdr (cdr (cdr cubo)))))))
  
  ;; Obtener las columnas que se van a mover
  (define columna-frontal (obtener-columna frontal columna tamaño))
  (define columna-superior (obtener-columna superior columna tamaño))
  (define columna-trasera-rev (obtener-columna trasera (- tamaño (- columna 1)) tamaño))
  (define columna-trasera (reverse columna-trasera-rev))
  (define columna-inferior (obtener-columna inferior columna tamaño))
  
  ;; Actualizar las caras
  (define nueva-cara-frontal (actualizar-columna frontal columna columna-inferior tamaño))
  (define nueva-cara-superior (actualizar-columna superior columna (reverse columna-frontal) tamaño))
  (define nueva-cara-trasera (actualizar-columna trasera (- tamaño (- columna 1)) (reverse columna-superior) tamaño))
  (define nueva-cara-inferior (actualizar-columna inferior columna columna-trasera tamaño))
  
  ;; Actualizar cara derecha o izquierda según la columna que se rota
  (define nueva-cara-lateral 
    (if (= columna 1)
        (rotar-cara-antihorario izquierda tamaño)
        (if (= columna tamaño)
            (rotar-cara-horario derecha tamaño)
            derecha)))
  
  ;; Construir nuevo cubo
  (cons nueva-cara-frontal
        (cons (if (= columna tamaño) nueva-cara-lateral derecha)
              (cons nueva-cara-trasera
                    (cons (if (= columna 1) nueva-cara-lateral izquierda)
                          (cons nueva-cara-superior
                                (cons nueva-cara-inferior '()))))))
  )

;; Función para mover una columna hacia abajo - MODIFICADA
(define (mover-columna-abajo cubo columna tamaño)
  ;; Extraer caras
  (define frontal (car cubo))
  (define derecha (car (cdr cubo)))
  (define trasera (car (cdr (cdr cubo))))
  (define izquierda (car (cdr (cdr (cdr cubo)))))
  (define superior (car (cdr (cdr (cdr (cdr cubo))))))
  (define inferior (car (cdr (cdr (cdr (cdr (cdr cubo)))))))
  
  ;; Obtener las columnas que se van a mover
  (define columna-frontal (obtener-columna frontal columna tamaño))
  (define columna-superior (obtener-columna superior columna tamaño))
  (define columna-trasera-rev (obtener-columna trasera (- tamaño (- columna 1)) tamaño))
  (define columna-trasera (reverse columna-trasera-rev))
  (define columna-inferior (obtener-columna inferior columna tamaño))
  
  ;; Actualizar las caras
  (define nueva-cara-frontal (actualizar-columna frontal columna columna-superior tamaño))
  (define nueva-cara-superior (actualizar-columna superior columna (reverse columna-trasera) tamaño))
  (define nueva-cara-trasera (actualizar-columna trasera (- tamaño (- columna 1)) (reverse columna-inferior) tamaño))
  (define nueva-cara-inferior (actualizar-columna inferior columna columna-frontal tamaño))
  
  ;; Actualizar cara derecha o izquierda según la columna que se rota
  (define nueva-cara-lateral 
    (if (= columna 1)
        (rotar-cara-horario izquierda tamaño)
        (if (= columna tamaño)
            (rotar-cara-antihorario derecha tamaño)
            derecha)))
  
  ;; Construir nuevo cubo
  (cons nueva-cara-frontal
        (cons (if (= columna tamaño) nueva-cara-lateral derecha)
              (cons nueva-cara-trasera
                    (cons (if (= columna 1) nueva-cara-lateral izquierda)
                          (cons nueva-cara-superior
                                (cons nueva-cara-inferior '()))))))
  )

;; Función para crear una cara con colores uniformes
(define (crear-cara color tamaño)
  (define (construir-cara color tamaño count)
    (if (= count (* tamaño tamaño))
        '()
        (cons color (construir-cara color tamaño (+ count 1)))))
  (construir-cara color tamaño 0))

;; Función para inicializar el cubo con tamaño variable
(define (inicializar-cubo tamaño)
  (cons (crear-cara 'w tamaño)         ;; Frontal (blanco)
        (cons (crear-cara 'g tamaño)   ;; Derecha (verde)
              (cons (crear-cara 'y tamaño) ;; Trasera (amarilla)
                    (cons (crear-cara 'b tamaño) ;; Izquierda (azul)
                          (cons (crear-cara 'r tamaño) ;; Superior (rojo)
                                (cons (crear-cara 'o tamaño) '())))))))  ;; Inferior (naranja)

;; Función para mostrar el cubo de tamaño variable
(define (mostrar-cubo cubo tamaño)
  (displayln "Estado actual del cubo:")
  
  ;; Extraer caras
  (define frontal (car cubo))
  (define derecha (car (cdr cubo)))
  (define trasera (car (cdr (cdr cubo))))
  (define izquierda (car (cdr (cdr (cdr cubo)))))
  (define superior (car (cdr (cdr (cdr (cdr cubo))))))
  (define inferior (car (cdr (cdr (cdr (cdr (cdr cubo)))))))
  
  ;; Función auxiliar para mostrar una fila
  (define (mostrar-fila cara fila col tamaño)
    (if (> col tamaño)
        (newline)
        (begin
          (display (obtener-elemento cara fila col tamaño))
          (display " ")
          (mostrar-fila cara fila (+ col 1) tamaño))))
  
  ;; Función para mostrar una cara normal
  (define (mostrar-cara-normal cara nombre tamaño)
    (displayln (string-append "Cara " nombre ":"))
    (define (mostrar-filas cara fila tamaño)
      (if (> fila tamaño)
          (void)
          (begin
            (mostrar-fila cara fila 1 tamaño)
            (mostrar-filas cara (+ fila 1) tamaño))))
    (mostrar-filas cara 1 tamaño))
  
  ;; Función específica para mostrar la cara superior invertida
  (define (mostrar-cara-superior-invertida cara nombre tamaño)
    (displayln (string-append "Cara " nombre ":"))
    ;; Recorremos las filas en orden inverso para la cara superior
    (define (mostrar-filas-invertidas cara fila tamaño)
      (if (< fila 1)
          (void)
          (begin
            (mostrar-fila cara fila 1 tamaño)
            (mostrar-filas-invertidas cara (- fila 1) tamaño))))
    (mostrar-filas-invertidas cara tamaño tamaño))
  
  ;; Mostrar todas las caras, con la superior usando la función específica
  (mostrar-cara-normal frontal "Frontal (Blanca)" tamaño)
  (mostrar-cara-normal derecha "Derecha (Verde)" tamaño)
  (mostrar-cara-normal trasera "Trasera (Amarilla)" tamaño)
  (mostrar-cara-normal izquierda "Izquierda (Azul)" tamaño)
  (mostrar-cara-superior-invertida superior "Superior (Rojo)" tamaño)
  (mostrar-cara-normal inferior "Inferior (Naranja)" tamaño))

;; Función para aplicar un movimiento (MODIFICADA para manejar símbolos)
(define (aplicar-movimiento cubo movimiento tamaño)
  (define mov-str (symbol->string movimiento))
  (define tipo-movimiento (substring mov-str 0 1))
  (define num (string->number (substring mov-str 1 (- (string-length mov-str) 1))))
  (define direccion (substring mov-str (- (string-length mov-str) 1)))
  
  (cond
    [(equal? tipo-movimiento "F") 
     (if (equal? direccion "D")
         (mover-fila-derecha cubo num tamaño)
         (mover-fila-izquierda cubo num tamaño))]
    [(equal? tipo-movimiento "C") 
     (if (equal? direccion "A")
         (mover-columna-arriba cubo num tamaño)
         (mover-columna-abajo cubo num tamaño))]
    [else cubo]))

;; Función recursiva para aplicar una secuencia de movimientos
(define (aplicar-secuencia cubo secuencia tamaño)
  (if (null? secuencia)
      cubo
      (aplicar-secuencia (aplicar-movimiento cubo (car secuencia) tamaño) (cdr secuencia) tamaño)))

;; NUEVA FUNCIÓN RS que reemplaza la anterior interfaz de usuario
(define (RS tamaño _ movimientos)
  (define cubo-inicial (inicializar-cubo tamaño))
  
  ;; Mostrar estado inicial
  (displayln "Estado inicial del cubo:")
  (mostrar-cubo cubo-inicial tamaño)
  
  ;; Aplicar los movimientos
  (define cubo-final (aplicar-secuencia cubo-inicial movimientos tamaño))
  
  ;; Mostrar estado final
  (displayln "\nEstado final después de aplicar los movimientos:")
  (mostrar-cubo cubo-final tamaño)
  
  ;; Devolver el cubo final
  )

;; Ejemplos de uso:
;; (RS 3 '([cubo]) '(F1D C2A F3I))
;; (RS 2 '([cubo]) '(F1D C1A))
