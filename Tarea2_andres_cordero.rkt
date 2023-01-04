(write "6)-------------------- ALGORITMO DE LUHN -----------------------------")(newline)
(define (pro para)
  (cond [(null? para) '()]
        [else(append (list(* (car para) 2)) (pro (cdr para)))]) )

(define (addimp l)
  (cond[(null? l) 0]
       [else(+ (car l) (addimp (cdr l)))]))

(define (addp l)
  (cond[(null? l) 0]
       [(> (car l) 9) (+ (- (car l) 9) (addp (cdr l)))]
       [else(+(car l)(addp (cdr l)))]))

(define (rev para)
  (if (null? para) '()
    (append (rev (cdr para))(list (car para)))))

(define(fun para)
  (+ (addp(pro (parap para)))
     (addimp(paraop para))))

(define (parap list)
  (cond[(null? (cdr list)) '()]
       [(null? (cddr list)) (cdr list)]
       [else (cons (car(cdr list)) (parap (cddr list)))]))

(define (paraop para)
  (cond[(null? (cdr para)) para]
       [(null? (cddr para)) (paraop (cons (car para) '()))]
       [else (append (list (car para)) (paraop (cddr para)))]))
							  

(define (Luhn number)
  (cond [(= (remainder (fun number) 10) 0) #t]
        [else #f]))

(define listapara6 (list 4 9 9 2 7 3 9 8 7 1 6))

(write "Lista a prueba: ")listapara6
(write "Usando el algoritmo de Luhn verificamos si es correcto: ")(Luhn listapara6)

