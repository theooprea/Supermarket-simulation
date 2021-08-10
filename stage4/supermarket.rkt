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
(define-struct counter (index tt et queue) #:transparent)

(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (map (λ (e) (if (= (counter-index e) index) (f e) e)) counters))

(define tt+
  (λ (C minutes)
    (match C
      [(counter index tt et queue)
       (make-counter index (+ tt minutes) et queue)])
    ))

(define et+
  (λ (C minutes)
    (match C
      [(counter index tt et queue)
       (make-counter index (+ tt minutes) (+ et minutes) queue)])
    ))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
      [(counter index tt et queue)
       (if (queue-empty? queue)
           (make-counter index (+ tt items) (+ et items) (enqueue (cons name items) queue))
           (make-counter index (+ tt items) et (enqueue (cons name items) queue))
           )])))

(define min-abstract
  (λ (counters min-index minim matching-func closed-counters)
    (if (empty? counters)
      (cons min-index minim)
      (match (car counters)
        [(counter index tt et queue)
         (if (and (< (matching-func (car counters)) minim) (not (member (counter-index (car counters)) closed-counters)))
             (min-abstract (cdr counters) index (matching-func (car counters)) matching-func closed-counters)
             (min-abstract (cdr counters) min-index minim matching-func closed-counters)
             )])
      )
    ))

(define min-tt
  (λ (counters closed-counters)
    (min-abstract counters 9999 9999 counter-tt closed-counters)
    ))

;(define min-tt
;  (λ (counters closed-counters)
;    (match (car counters)
;    [(counter index tt et queue)
;     (min-abstract (cdr counters) index tt counter-tt closed-counters)])
;    )) ; folosind funcția de mai sus

(define min-et
  (λ (counters)
    (match (car counters)
    [(counter index tt et queue)
     (min-abstract (cdr counters) index et counter-et)])
    ))

(define most-advanced-client
  (λ (counters min-index minim)
    (if (empty? counters)
      (cons min-index minim)
      (match (car counters)
        [(counter index tt et queue)
         (if (and (< (counter-et (car counters)) minim) (not (queue-empty? queue)))
             (most-advanced-client (cdr counters) index (counter-et (car counters)))
             (most-advanced-client (cdr counters) min-index minim)
             )])
      )
    ))

(define (remove-first-from-counter C)
  (match C
    [(counter index tt et queue)
     (if (or (queue-empty? queue) (queue-empty? (dequeue queue)))
         (make-counter index 0 0 empty-queue)
         (make-counter index (- tt et) (cdr (top (dequeue queue))) (dequeue queue))
         )]))

(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
      [(counter index tt et queue)
       (if (<= tt minutes)
           (make-counter index 0 0 queue)
           (if (<= et minutes)
               (make-counter index (- tt minutes) 0 queue)
               (make-counter index (- tt minutes) (- et minutes) queue)
               )
           )])))
  
  
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
(define (average L closed)
  (/ (apply + L) (length L)))

(define (ensure-aux fast-counters slow-counters minutes closed)
  (if (> (average (map (λ (e) (match e [(counter index tt et queue) tt])) (filter (λ (e) (not (member (counter-index e) closed))) (append fast-counters slow-counters))) closed) minutes)
      (ensure-aux fast-counters (append slow-counters (list (empty-counter (+ 1 (length fast-counters) (length slow-counters))))) minutes closed)
      slow-counters
      ))

(define (serve-aux requests fast-counters slow-counters already-served closed)
  (if (null? requests)
      (append (list already-served) (map (λ (e) (cons (counter-index e) (counter-queue e)))(filter (λ (e) (not (queue-empty? (counter-queue e)))) fast-counters)) (map (λ (e) (cons (counter-index e) (counter-queue e)))(filter (λ (e) (not (queue-empty? (counter-queue e)))) slow-counters)))
      ;(append (list already-served) fast-counters slow-counters) ;for debug
      ;closed ; debug
      (match (car requests)
        [(list 'delay index minutes)
         (if (<= index (length fast-counters))
             (serve-aux (cdr requests) (map (λ (e) (if (= (counter-index e) index) (et+ e minutes) e)) fast-counters) slow-counters already-served closed)
             (serve-aux (cdr requests) fast-counters (map (λ (e) (if (= (counter-index e) index) (et+ e minutes) e)) slow-counters) already-served closed)
             )]
        [(list 'ensure minutes)
         (serve-aux (cdr requests) fast-counters (ensure-aux fast-counters slow-counters minutes closed) already-served closed)]
        [(list 'close index)
         (serve-aux (cdr requests) fast-counters slow-counters already-served (cons index closed))]
        [(list name n-items)
         (if (<= n-items ITEMS)
             (if (<= (car (min-tt (append fast-counters slow-counters) closed)) (length fast-counters))
                 (serve-aux (cdr requests) (map (λ (e) (if (= (counter-index e) (car (min-tt fast-counters closed))) ((add-to-counter name n-items) e) e)) fast-counters) slow-counters already-served closed)
                 (serve-aux (cdr requests) fast-counters (map (λ (e) (if (= (counter-index e) (car (min-tt slow-counters closed))) ((add-to-counter name n-items) e) e)) slow-counters) already-served closed)
                 )
             (serve-aux (cdr requests) fast-counters (map (λ (e) (if (= (counter-index e) (car (min-tt slow-counters closed))) ((add-to-counter name n-items) e) e)) slow-counters) already-served closed)
             )]
        [minutes
         (if (= (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999)) 9999)
             (serve-aux (cdr requests) (map (λ (e) ((pass-time-through-counter minutes) e)) fast-counters) (map (λ (e) ((pass-time-through-counter minutes) e)) slow-counters) already-served closed)
             (if (< minutes (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999)))
                    (serve-aux (cdr requests) (map (λ (e) ((pass-time-through-counter minutes) e)) fast-counters) (map (λ (e) ((pass-time-through-counter minutes) e)) slow-counters) already-served closed)
                    (if (= minutes (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999)))
                        (serve-aux
                         (cdr requests)    ;requests
                         (map (λ (e) (if (= (counter-et e) minutes) (remove-first-from-counter e) ((pass-time-through-counter minutes) e))) fast-counters)    ;fast coutners
                         (map (λ (e) (if (= (counter-et e) minutes) (remove-first-from-counter e) ((pass-time-through-counter minutes) e))) slow-counters)    ;slow counters
                         (append already-served (map (λ (e) (cons (counter-index e) (car (top (counter-queue e))))) (filter (λ (e) (not (queue-empty? (counter-queue e)))) (filter (λ (e) (= (counter-et e) minutes)) (append fast-counters slow-counters)))))    ;already-served
                         closed)
                        (serve-aux
                         (cons (- minutes (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) (cdr requests))    ;requests
                         (map (λ (e) (if (= (counter-et e) (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) (remove-first-from-counter e) ((pass-time-through-counter (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) e))) fast-counters)   ;fast coutners
                         (map (λ (e) (if (= (counter-et e) (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) (remove-first-from-counter e) ((pass-time-through-counter (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) e))) slow-counters)   ;slow counters
                         (append already-served (map (λ (e) (cons (counter-index e) (car (top (counter-queue e))))) (filter (λ (e) (not (queue-empty? (counter-queue e)))) (filter (λ (e) (= (counter-et e) (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999)))) (append fast-counters slow-counters)))))   ;already-served
                         closed)
                        )
                    )
             )]
        
        ))
      )

(define (serve requests fast-counters slow-counters)
  (serve-aux requests fast-counters slow-counters null null))