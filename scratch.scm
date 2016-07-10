#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;;definition for questions association list calling it "responses"
(define responses
  '((1 "What type of films do you like?")
    (2 "So you like gore?" )
    (3 "Shall I recommend a gory film for you?")
    (4 "Shall I recommend a non-gory scary film for you?")))

    
;;definition for possible answers association list calling it "decisiontable"
(define decisiontable
  '((1 ((comedy) 20) ((very scary) 2) ((thrillers) 6) ((not animated) 5) ((horror) 2) ((scfi) 4))
    (2 ((some) 8) ((a lot) 7) ((yes) 7) ((no) 8) ((not really) 8))
    (3 ((yes) gory) ((ok) gory) ((no) 0))
    (4 ((yes) non-gory) ((ok) gory) ((no) 0))))

;;definition for procedure "assq-ref", that receives list of responses and id number from get-response
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))         ;and by using the assq pair selector(returns the first element of responses list of which car is equal to id)
                                    ;returns reminder of the pair as a list to get-response.                               

;;definition for procedure "assv-ref"
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

;;definition for procedure "get-response" receives id from procedure responses
(define (get-response id)       
  (car (assq-ref responses id))) ;passes a list of responses and id number to assq-ref, and then returns the list from
                                 ;assq-ref as an atom into the loop from recommend 

;;definition for procedure "get-keywords" that takes users answers
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (λ (key) (car key)) keys)))

;;definition for procedure "list-of-lengths"
;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (λ (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

;;definition for procedure "index-of-largest-number"
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (λ (x) (eq? x n)) list-of-numbers))))

;;definition for procedure "lookup"
(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))


;;definition for procedure "recommend"
(define (recommend initial-id)
  (let loop ((id initial-id))                   ;Procedure loop
    (format #t "~a\n> " (get-response id))      ;Print out a response based on the id value 
    (let* ((input (read-line))                  ;get and store user input as input with the use of read-line procedure
           (string-tokens (string-tokenize input)) ;split the user input into the string-tokens list of substrings using string-tokenize
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        ;;conditional clause
        (cond ((eq? #f response) ;;conditional predicate cond(same as "if")
	       (format #t "huh? I didn't understand that! ") ;;conditional action
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


(recommend 2)
