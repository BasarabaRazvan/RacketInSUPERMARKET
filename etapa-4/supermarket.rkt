#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index status tt et queue) #:transparent)

  
(define (empty-counter index)
  (make-counter index 1 0 0 empty-queue))

(define (update f counters index)
  (foldr (λ (element counters-acc)
         (if (equal? (counter-index element) index) (cons (f element) counters-acc) (cons element counters-acc)))
         '()
         counters))

(define switch-status
  (λ (C)
    (cond
      ((= (counter-status C) 1) (make-counter (counter-index C) 0 (counter-tt C) (counter-et C) (counter-queue C)))
      (else C)
  )))

(define tt+
  (λ (minutes)
    (λ (C)
      (make-counter (counter-index C) (counter-status C) (+ (match C [(counter index status tt et queue) tt]) minutes) (counter-et C) (counter-queue C)))))

(define et+
   (λ (minutes)
    (λ (C)
      (make-counter (counter-index C) (counter-status C) (counter-tt C) (+ (match C [(counter index status tt et queue) et]) minutes) (counter-queue C)))))


(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (cond
      ((queue-empty? (counter-queue C)) (make-counter (counter-index C) (counter-status C) (+ (match C [(counter index status tt et queue) tt]) items)
                                                                 (+ (match C [(counter index status tt et queue) et]) items) (enqueue (cons name items)  (counter-queue C))))
      (else (make-counter (counter-index C) (counter-status C) (+ (match C [(counter index status tt et queue) tt]) items)
                     (match C [(counter index status tt et queue) et])  (enqueue (cons name items) (counter-queue C)))))
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
    ((= (+ (queue-size-r (counter-queue C)) (queue-size-l (counter-queue C))) 1) (make-counter (counter-index C) (counter-status C) 0 0 empty-queue))
    (else (make-counter (counter-index C) (counter-status C) (if (< (counter-et C) (cdr (top (counter-queue C)))) (- (counter-tt C) (cdr (top (counter-queue C))))
                                      (- (counter-tt C) (counter-et C))) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C))))
    ))


(define (remove-first-aux C)
  (cond
    ((queue-empty? (counter-queue C)) (make-counter (counter-index C) (counter-status C) 0 0 (counter-queue C)))
    (else (remove-first-from-counter C))
  ))


(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
      ((queue-empty? (counter-queue C)) (make-counter (counter-index C) (counter-status C) (if (> (- (counter-tt C) minutes) 0) (- (counter-tt C) minutes) 0)
                                                      (if (> (- (counter-et C) minutes) 0) (- (counter-et C) minutes) 0) (counter-queue C)))
      (else (make-counter (counter-index C) (counter-status C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C))))
    ))


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


(define (list->stream l)
  (if (null? l)
      empty-stream
      (stream-cons (car l) (list->stream (cdr l)))))

(define (new->legacy q)
  (struct-copy queue q
               [left (stream->list (queue-left q))]))

(define (friendly-output output)
  (cons (car output)
        (map (λ (pair) (cons (car pair) (new->legacy (cdr pair))))
             (cdr output))))
					
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (serve requests fast-counters slow-counters)
  (serve-help requests fast-counters slow-counters '()))


(define (serve-help requests fast-counters slow-counters acc)
  (if (null? requests)
    (cons acc (foldr (λ (el cozi) (cons (cons (counter-index el) (counter-queue el)) cozi)) null
                     (filter (λ (el) (not (queue-empty? (counter-queue el)))) (append fast-counters slow-counters))))
    (match (car requests)
        [(list 'ensure average)
         (cond
           ((< average (/ (car (suma (append fast-counters slow-counters))) (cdr (suma (append fast-counters slow-counters)))))
            (serve-help requests fast-counters (append slow-counters (list (empty-counter (+ (cdr (suma (append fast-counters slow-counters))) 1)))) acc))
           (else (serve-help (cdr requests) fast-counters slow-counters acc)))
           
        ]
        [(list 'close index)
         (serve-help (cdr requests) (update switch-status fast-counters index) (update switch-status slow-counters index) acc)
        ]
        [(list 'delay index minutes)
         (serve-help (cdr requests) (update (λ (el) ((tt+ minutes) el)) (update (λ (el) ((et+ minutes) el)) fast-counters index) index)
                                     (update (λ (el) ((tt+ minutes) el)) (update (λ (el) ((et+ minutes) el)) slow-counters index) index) acc)
        ]
        [(list name n-items)
         (cond
           ((<= n-items ITEMS) (serve-help (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (filter (λ (el) (= (counter-status el) 1))
                                                                                                                                   (append fast-counters slow-counters)))))
                                                     (update (add-to-counter name n-items) slow-counters (car (min-tt (filter (λ (el) (= (counter-status el) 1))
                                                                                                                              (append fast-counters slow-counters))))) acc))
           (else (serve-help (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) acc))
           )
        ]
        [minutes
         (serve-help (cdr requests) (map (pass-time-through-counter-helper minutes) fast-counters)
                     (map (pass-time-through-counter-helper minutes) slow-counters) (if (null? (serve-h minutes (append fast-counters slow-counters)))
                                                                                        acc (append acc (serve-h minutes (append fast-counters slow-counters)))))
        ]
      )))

