#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (foldr (λ (element counters-acc)
         (if (equal? (counter-index element) index) (cons (f element) counters-acc) (cons element counters-acc)))
         '()
         counters))

(define tt+
  (λ (minutes)
    (λ (C)
      (make-counter (counter-index C) (+ (match C [(counter index tt et queue) tt]) minutes) (counter-et C) (counter-queue C)))))

(define et+
   (λ (minutes)
    (λ (C)
      (make-counter (counter-index C) (counter-tt C) (+ (match C [(counter index tt et queue) et]) minutes) (counter-queue C)))))


(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (cond
      ((queue-empty? (counter-queue C)) (make-counter (counter-index C) (+ (match C [(counter index tt et queue) tt]) items)
                                                                 (+ (match C [(counter index tt et queue) et]) items) (enqueue (cons name items)  (counter-queue C))))
      (else (make-counter (counter-index C) (+ (match C [(counter index tt et queue) tt]) items)
                     (match C [(counter index tt et queue) et])  (enqueue (cons name items) (counter-queue C)))))
    ))


(define (min-tt-or-et f counters)
  (cond
    ((null? counters) (cons null null))
    (else (foldl (λ (element acc) (if (< (f element) (cdr acc))
                                      (cons (counter-index element) (f element)) acc))
                   (cons (counter-index (car counters)) (f (car counters)))
                   counters))
  ))


(define (min-tt counters)
  (min-tt-or-et (λ (counter) (counter-tt counter)) counters))


(define (min-et counters)
  (min-tt-or-et (λ (counter) (counter-et counter)) counters))


(define (remove-first-from-counter C)   ; testată de checker
  (cond
    ((= (+ (queue-size-r (counter-queue C)) (queue-size-l (counter-queue C))) 1) (make-counter (counter-index C) 0 0 empty-queue))
    (else (make-counter (counter-index C) (if (< (counter-et C) (cdr (top (counter-queue C)))) (- (counter-tt C) (cdr (top (counter-queue C))))
                                      (- (counter-tt C) (counter-et C))) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C))))
    ))

(define (remove-first-aux C)
  (cond
    ((queue-empty? (counter-queue C)) (make-counter (counter-index C) 0 0 (counter-queue C)))
    (else (remove-first-from-counter C))
  ))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
      ((queue-empty? (counter-queue C)) (make-counter (counter-index C) (if (> (- (counter-tt C) minutes) 0) (- (counter-tt C) minutes) 0)
                                                      (if (> (- (counter-et C) minutes) 0) (- (counter-et C) minutes) 0) (counter-queue C)))
      (else (make-counter (counter-index C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C))))
    ))


(define acc '())
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
  (serve-help requests fast-counters slow-counters '()))


(define (serve-help requests fast-counters slow-counters acc)
  (if (null? requests)
    (cons acc (append fast-counters slow-counters))
    (match (car requests)
        [(list 'ensure average)
         (cond
           ((< average (/ (car (suma (append fast-counters slow-counters))) (cdr (suma (append fast-counters slow-counters)))))
            (serve-help requests fast-counters (append slow-counters (list (empty-counter (+ (cdr (suma (append fast-counters slow-counters))) 1)))) acc))
           (else (serve-help (cdr requests) fast-counters slow-counters acc)))
           
        ]
        [(list 'delay index minutes)
         (serve-help (cdr requests) (update (λ (el) ((tt+ minutes) el)) (update (λ (el) ((et+ minutes) el)) fast-counters index) index)
                                     (update (λ (el) ((tt+ minutes) el)) (update (λ (el) ((et+ minutes) el)) slow-counters index) index) acc)
        ]
        [(list name n-items)
         (cond
           ((<= n-items ITEMS) (serve-help (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters))))
                                                     (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters slow-counters)))) acc))
           (else (serve-help (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) acc))
           )
        ]
        [minutes
         (serve-help (cdr requests) (map (pass-time-through-counter-helper minutes) fast-counters)
                     (map (pass-time-through-counter-helper minutes) slow-counters) (if (null? (serve-h minutes (append fast-counters slow-counters)))
                                                                                        acc (append acc (serve-h minutes (append fast-counters slow-counters)))))
        ]
      )))


(define (serve-h minutes counters)
  (foldr (λ (element acc)
            (cond
             ((and (>= minutes (counter-et element)) (not (queue-empty? (counter-queue element)))) (cons (cons (counter-index element) (car (top (counter-queue element)))) acc))
             (else acc)
             ))
         '()
         counters
  ))


(define (pass-time-through-counter-helper minutes)
  (λ (C)
    (cond
      ((or (= minutes 0) (= 0 (counter-et C))) C)
      ((< minutes (counter-et C)) ((pass-time-through-counter minutes) C))
      (else ((pass-time-through-counter-helper (- minutes (counter-et C))) (remove-first-aux C)))
  )))


(define (suma counters)
  (suma-h counters 0 0))


(define (suma-h counters sum count)
  (cond
    ((null? counters) (cons sum count))
    (else (suma-h (cdr counters) (+ sum (counter-tt (car counters))) (+ count 1))))
  )