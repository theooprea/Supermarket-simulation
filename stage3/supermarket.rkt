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
  (λ (counters min-index minim matching-func)
    (if (empty? counters)
      (cons min-index minim)
      (match (car counters)
        [(counter index tt et queue)
         (if (< (matching-func (car counters)) minim)
             (min-abstract (cdr counters) index (matching-func (car counters)) matching-func)
             (min-abstract (cdr counters) min-index minim matching-func)
             )])
      )
    ))

(define min-tt
  (λ (counters)
    (match (car counters)
    [(counter index tt et queue)
     (min-abstract (cdr counters) index tt counter-tt)])
    )) ; folosind funcția de mai sus

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

(define (remove-first-from-counter C)   ; testată de checker
  (match C
    [(counter index tt et queue)
     (if (or (queue-empty? queue) (queue-empty? (dequeue queue)))
         (make-counter index 0 0 empty-queue)
         (make-counter index (- tt et) (cdr (top (dequeue queue))) (dequeue queue))
         )]))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
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

(define (average L)
  (/ (apply + L) (length L)))

(define (ensure-aux fast-counters slow-counters minutes)
  (if (> (average (map (λ (e) (match e [(counter index tt et queue) tt])) (append fast-counters slow-counters))) minutes)
      (ensure-aux fast-counters (append slow-counters (list (empty-counter (+ 1 (length fast-counters) (length slow-counters))))) minutes)
      slow-counters
      ))

(define (serve-aux requests fast-counters slow-counters already-served)
  (if (null? requests)
      (append (list already-served) fast-counters slow-counters)
      (match (car requests)
        [(list 'delay index minutes)
         (if (<= index (length fast-counters))
             (serve-aux (cdr requests) (map (λ (e) (if (= (counter-index e) index) (et+ e minutes) e)) fast-counters) slow-counters already-served)
             (serve-aux (cdr requests) fast-counters (map (λ (e) (if (= (counter-index e) index) (et+ e minutes) e)) slow-counters) already-served)
             )]
        [(list 'ensure minutes)
         (serve-aux (cdr requests) fast-counters (ensure-aux fast-counters slow-counters minutes) already-served)]
        [(list name n-items)
         (if (<= n-items ITEMS)
             (if (<= (car (min-tt (append fast-counters slow-counters))) (length fast-counters))
                 (serve-aux (cdr requests) (map (λ (e) (if (= (counter-index e) (car (min-tt fast-counters))) ((add-to-counter name n-items) e) e)) fast-counters) slow-counters already-served)
                 (serve-aux (cdr requests) fast-counters (map (λ (e) (if (= (counter-index e) (car (min-tt slow-counters))) ((add-to-counter name n-items) e) e)) slow-counters) already-served)
                 )
             (serve-aux (cdr requests) fast-counters (map (λ (e) (if (= (counter-index e) (car (min-tt slow-counters))) ((add-to-counter name n-items) e) e)) slow-counters) already-served)
             )]
        [minutes
         (if (= (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999)) 9999)
             (serve-aux (cdr requests) (map (λ (e) ((pass-time-through-counter minutes) e)) fast-counters) (map (λ (e) ((pass-time-through-counter minutes) e)) slow-counters) already-served)
             (if (< minutes (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999)))
                    (serve-aux (cdr requests) (map (λ (e) ((pass-time-through-counter minutes) e)) fast-counters) (map (λ (e) ((pass-time-through-counter minutes) e)) slow-counters) already-served)
                    (if (= minutes (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999)))
                        (serve-aux
                         (cdr requests)    ;requests
                         (map (λ (e) (if (= (counter-et e) minutes) (remove-first-from-counter e) ((pass-time-through-counter minutes) e))) fast-counters)    ;fast coutners
                         (map (λ (e) (if (= (counter-et e) minutes) (remove-first-from-counter e) ((pass-time-through-counter minutes) e))) slow-counters)    ;slow counters
                         (append already-served (map (λ (e) (cons (counter-index e) (car (top (counter-queue e))))) (filter (λ (e) (not (queue-empty? (counter-queue e)))) (filter (λ (e) (= (counter-et e) minutes)) (append fast-counters slow-counters)))))    ;already-served
                         )
                        (serve-aux
                         (cons (- minutes (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) (cdr requests))    ;requests
                         (map (λ (e) (if (= (counter-et e) (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) (remove-first-from-counter e) ((pass-time-through-counter (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) e))) fast-counters)   ;fast coutners
                         (map (λ (e) (if (= (counter-et e) (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) (remove-first-from-counter e) ((pass-time-through-counter (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999))) e))) slow-counters)   ;slow counters
                         (append already-served (map (λ (e) (cons (counter-index e) (car (top (counter-queue e))))) (filter (λ (e) (not (queue-empty? (counter-queue e)))) (filter (λ (e) (= (counter-et e) (cdr (most-advanced-client (append fast-counters slow-counters) 9999 9999)))) (append fast-counters slow-counters)))))   ;already-served
                         )
                        )
                    )
             )]
        )
      ))

(define (serve requests fast-counters slow-counters)
  (serve-aux requests fast-counters slow-counters '()))