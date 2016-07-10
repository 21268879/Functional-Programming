#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;Definition for "responses" association list.
(define responses
  '((1 "What type of films do you like?")
    (2 "So you like gore?" )
    (3 "Shall I recommend a gory film for you?")
    (4 "Shall I recommend a non-gory scary film for you?")))

    
;Definition for "decisiontable" association list for possible answers from user.
(define decisiontable
  '((1 ((comedy) 20) ((very scary) 2) ((thrillers) 6) ((not animated) 5) ((horror) 2) ((scfi) 4))
    (2 ((some) 8) ((a lot) 7) ((yes) 7) ((no) 8) ((not really) 8))
    (3 ((yes) gory) ((ok) gory) ((no) 0))
    (4 ((yes) non-gory) ((ok) gory) ((no) 0))
    )
  )

;Definition for "assq-ref"procedure, that receives list of responses and "id" number from "get-response" procedure.
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))         ;By using the "assq" pair selector(returns the first element of "responses" list of which car is equal to the "id" supplied)
                                    ;returns remainder of the pair as a list to "get-response" procedure.                               

;Definition for "assv-ref" procedure.
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

;Definition for "get-response" procedure which receives the "id" from "responses" procedure.
(define (get-response id)       
  (car (assq-ref responses id))) ;Passes a list of responses and the "id" number to "assq-ref" procedure, 
                                 ;then returns the list from "assq-ref" as an atom into the loop from "recommend". 

;Definition for "get-keywords" procedure that takes users answers for an argument.
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (λ (key) (car key)) keys)))

;Definition for "list-of-lengths" procedure, that outputs a list in the form: (0 0 0 2 0 0).
(define (list-of-lengths keylist tokens)
  (map 
   (λ (x)
     (let ((set (lset-intersection eq? tokens x)))
       ; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

;Definition for "index-of-largest-number" procedure.
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (λ (x) (eq? x n)) list-of-numbers))))

;Definition for "lookup" procedure.
(define (lookup id tokens) ;Takes in the "id" and the "response" from "recommend" procedure,
  (let* ((record (assv-ref decisiontable id)) ;creates the "record" indentifier that stores the results from the "assv-ref" procedure
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))


;Definition for "recommend" procedure.
(define (recommend initial-id)
  (let loop ((id initial-id))                   
    (format #t "~a\n> " (get-response id))      ;Prints out a response based on the "id" value 
    (let* ((input (read-line))                  ;,with the use of "read-line" procedure gets the user input. Use of sequential binding let* store the user input in the "input" identifier.
           (string-tokens (string-tokenize input)) ;"string-tokenize" procedure splits the user input into "string-tokens" list of substrings
           (tokens (map string->symbol string-tokens))) ;the "map" procedure applies the "string->symbol" procedure to the supplied list and creates a mutable string out of each "string-tokens" substring.
      (let ((response (lookup id tokens)))
        ;conditional clause
        (cond ((eq? #f response) ;conditional predicate cond(same as "if")
	       (format #t "huh? I didn't understand that! ") ;conditional action
	       (loop id)) 
	      ((eq? 'gory response)
	       (format #t "Searching for gory horror films ....\n")
	       (exit))
 	      ((eq? 'non-gory response)
	       (format #t "Searching for non-gory scarey films ....\n")
	       (exit))             
	      ((zero? response)
	       (format #t "So Long, and Thanks for All the Fish...\n")
	       (exit))
	      (else
	       (loop response)))))))

(recommend 1)
