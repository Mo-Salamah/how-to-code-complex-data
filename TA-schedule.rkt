;; Final project of the course How to Code: Complex Data


;  PROBLEM 1:
;
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people.
;
;  Design a data definition for Chirper, including a template that is tail recursive and avoids
;  cycles.
;
;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.
;


(define-struct user (name verified follows))
;; (make-user String Boolean (listof User)
;; interp. A Chirper user, their name, their
;; verification status, and users followed

(define U1 (make-user "U1" true empty))
(define U2 (make-user "U2" false (list U1)))
(define U3 (make-user "U3" true (list U1 U2)))

#; 
(define (fn-for-user u)   ;;*mutually recursive data definitions need two functions*
  ;; worklist is (listof User); a worklist accumulator
  ;; visited is (listof String); maintains a listed of visited users
  
  (local [
          (define (fn-for-user--u u worklist visited)
            (if (member (user-name u) visited)
                (fn-for-user--lou worklist visited)
                (fn-for-user--lou (append (user-follows u) worklist) (cons (user-name u) visited))))   ; ... (user-name u) (user-verified u)
               
          (define (fn-for-user-lou worklist)
            (cond [(empty? lou) (...)]
                  [else (...
                         (fn-for-user--u (first worklist) (rest worklist) visited))]))]
    (fn-for-user--u u)))



;  PROBLEM 2:
;
;  In UBC's version of How to Code, there are often more than 800 students taking
;  the course in any given semester, meaning there are often over 40 Teaching Assistants.
;
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
;  a program to do it for us!
;
;  Below are some data definitions for a simplified version of a TA schedule. There are some
;  number of slots that must be filled, each represented by a natural number. Each TA is
;  available for some of these slots, and has a maximum number of shifts they can work.
;
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;  maximum shifts. If no such schedules exist, produce false.
;
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))
( define SOBA2 (make-ta "Soba" 2 (list 1 3 4)))

(define NOODLE-TAs (list SOBA UDON RAMEN))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)

(define-struct tuple (ta bool))
;; (make-tuple String Boolean)
;; interp. a tuple containing an available TA and
;; a boolean

;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible


; (define SOBA (make-ta "Soba" 2 (list 1 3)))
; (define UDON (make-ta "Udon" 1 (list 3 4)))
; (define RAMEN (make-ta "Ramen" 1 (list 2)))



(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false) 

(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)  ;;;;
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4))  ;;;;
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list  2 3 4))    ;;;;
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)))

(check-expect (schedule-tas NOODLE-TAs (list 2 4))     ;;;;
              (list
               (make-assignment UDON 4)
               
               (make-assignment RAMEN 2)))
 
(check-expect (schedule-tas NOODLE-TAs (list 3 4))     ;;;;;
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3) 
               
               ))

(check-expect (schedule-tas NOODLE-TAs (list 2 4))   ;;;;
              (list
               (make-assignment UDON 4)
               
               (make-assignment RAMEN 2)))

(check-expect (schedule-tas NOODLE-TAs (list 1 4))   ;;;;
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)


;(define (schedule-tas tas slots) empty) ;stub

(define (schedule-tas tas slots)
  ;; rsf is (listof Assignment); a result so far accumulator
  
  (local [(define (schedule-tas slots rsf tas)
            (cond [(empty? slots) rsf]
                  [(empty? tas) false]
                  [else (local [(define try (someone-available? tas (first slots) rsf))]   ;try is (make-tuple ta bool)
                          (if (tuple-bool try)
                              (schedule-tas    
                               (rest slots)
                               (cons (make-assignment (tuple-ta try) (first slots)) rsf)
                               tas) 
                              false))]))]
    (schedule-tas slots empty tas)))

 

;; (listof tas) Slot (list Assignment) -> Tuple
;; produces tuple with an available TA and true or empty and false
(check-expect (someone-available? NOODLE-TAs 1 empty) (make-tuple SOBA true))

(check-expect (someone-available? NOODLE-TAs 2 (list
                                                (make-assignment UDON 4)
                                                (make-assignment SOBA 3)
                                                (make-assignment RAMEN 2)
                                                (make-assignment SOBA 1)))
              (make-tuple empty false))


(check-expect (someone-available? NOODLE-TAs 3 (list
                                                (make-assignment UDON 4)
                                                (make-assignment SOBA 3)
                                                (make-assignment RAMEN 2)
                                                (make-assignment SOBA 1)))
              (make-tuple empty false))

(check-expect (someone-available? NOODLE-TAs 8 (list
                                                (make-assignment UDON 4)
                                                (make-assignment SOBA 3)
                                                (make-assignment RAMEN 2)
                                                (make-assignment SOBA 1)))
              (make-tuple empty false))


; (define SOBA (make-ta "Soba" 2 (list 1 3)))
; (define UDON (make-ta "Udon" 1 (list 3 4)))
; (define RAMEN (make-ta "Ramen" 1 (list 2)))


(check-expect (someone-available? (list SOBA) 1 empty) (make-tuple SOBA true))
;(check-expect (someone-available? (list SOBA) 3 empty) (make-tuple SOBA true))

(check-expect (someone-available? (list SOBA) 1 (list (make-assignment SOBA 3))) (make-tuple SOBA true))
 


(check-expect (available? SOBA 3 empty) true)
(check-expect (someone-available? (list SOBA) 3 empty) (make-tuple SOBA true))


 

;(define (someone-available? tas slot) (make-tuple (make-ta "Name" 0 empty) false))

(define (someone-available? tas slot rsf)     ;
  (cond [(empty? tas) (make-tuple empty false)]
        [else (if (available? (first tas) slot rsf)
                  (make-tuple (first tas) true)
                  (someone-available? (rest tas) slot rsf))]))


;; TA Slot (list Assignment) -> Boolean
;; produce true if TA is available during Slot
(check-expect (available? SOBA 1 empty) true)

(check-expect (available? SOBA 1 (list
                                  (make-assignment UDON 4)
                                  (make-assignment SOBA 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA 1))) false)

(check-expect (available? (make-ta "Soba" 3 (list 1 3 4)) 3 (list   ;double-booked? violation
                                  (make-assignment UDON 4)
                                  (make-assignment (make-ta "Soba" 3 (list 1 3 4)) 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment (make-ta "Soba" 3 (list 1 3 4)) 1))) false)
 
(check-expect (available? (make-ta "Soba" 2 (list 1 3 4)) 4 (list   ;overworked? violation
                                  (make-assignment UDON 4)
                                  (make-assignment SOBA2 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA2 1))) false)
 
(check-expect (available? RAMEN 4 (list                          ;overworked? violation
                                  (make-assignment UDON 4)
                                  (make-assignment SOBA 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA 1))) false)

(check-expect (available? (make-ta "Soba" 2 (list 2 4)) 3 (list   ;double-booked? violation and overworked
                                  (make-assignment UDON 4)
                                  (make-assignment SOBA 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA 1))) false)

(check-expect (available? (make-ta "Soba" 2 (list 4 2)) 3 (list   ;double-booked? violation and overworked
                                  (make-assignment UDON 4)
                                  (make-assignment SOBA 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA 1))) false)

(check-expect (available? (make-ta "Soba" 2 (list 1 3 4)) 3 (list   ;double-booked? violation only
                                  (make-assignment UDON 4)
                                  (make-assignment SOBA2 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA2 1))) false) 


(check-expect (available? UDON 1 (list
                                  (make-assignment UDON 4)
                                  (make-assignment SOBA 3) 
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA 1))) false)
(check-expect (available? UDON 3 (list
               
                                  (make-assignment SOBA 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA 1))) true)



(check-expect (available? (make-ta "Soba" 2 (list 1 3 4)) 4 (list   ;overworked? violation
                                  (make-assignment UDON 4)
                                  (make-assignment SOBA2 3)
                                  (make-assignment RAMEN 2)
                                  (make-assignment SOBA2 1))) false)


(check-expect (available? SOBA 3 empty) true)
(check-expect (someone-available? (list SOBA) 3 empty) (make-tuple SOBA true))



(define (available? ta slot rsf)                    ;;***when debugging, don't write tests mindlessly
  
  (local [ (define (available? ta slot rsf acc avail)
  (cond [(or (= 0 (ta-max ta)) (empty? avail)) false]
        [else (if (equal? slot (first avail))
                  (if (and (not (double-booked? ta slot rsf)) (not (overworked? ta rsf)))     
                      true
                      false)
                  (available? ta slot rsf (add1 acc) (rest avail)))]))]
    
    (available? ta slot rsf 0 (ta-avail ta))))
 
  


;; TA (listof Assignment) -> Boolean
;; returns true if TA's mentions in loa is greater than max
;; or if one of TA's mentions in loa is for Slot
(check-expect (overworked? SOBA (list
                                 (make-assignment UDON 4)
                                 (make-assignment SOBA 3)
                                 (make-assignment RAMEN 2)
                                 (make-assignment SOBA 1))) true)
(check-expect (overworked? SOBA (list
                                 (make-assignment UDON 4)
                                 (make-assignment SOBA 3)
                                 (make-assignment RAMEN 2))) false) 
(check-expect (overworked? SOBA (list
                                 (make-assignment UDON 4)
                                 (make-assignment RAMEN 2))) false)

(check-expect (overworked? UDON (list
                                 
                                 (make-assignment SOBA 3)
                                 (make-assignment RAMEN 2)
                                 (make-assignment UDON 3)
                                 (make-assignment SOBA 1))) true)

(check-expect (overworked? RAMEN (list
                                 (make-assignment UDON 4)
                                 (make-assignment SOBA 3)
                                 (make-assignment RAMEN 2)
                                 (make-assignment SOBA 1))) true)

(check-expect (overworked? (make-ta "Ramen" 2 (list 2)) (list
                                 (make-assignment UDON 4)
                                 (make-assignment SOBA 3)
                                 (make-assignment RAMEN 2)
                                 (make-assignment SOBA 1))) false)


(define (overworked? ta rsf)
  ;; acc is Natural; result so far accumulator counting ta's # of occurences 
  (local [
          (define (overworked? ta rsf acc)
            (cond [(empty? rsf) acc]
                  [else (if (equal? ta (assignment-ta (first rsf)))
                            (overworked? ta (rest rsf) (add1 acc))       
                            (overworked? ta (rest rsf)  acc))]))]
    
    
    (< (- (ta-max ta) (overworked? ta rsf 0)) 1)))
 
  
;; TA Slot (listof Assignment) -> Boolean
;; produce true if (make-assignment TA Slot) appears in loa
(check-expect (double-booked? SOBA 3 empty) false)

(check-expect (double-booked? SOBA 2 (list
               
                                      (make-assignment RAMEN 2))) false)

(check-expect (double-booked? SOBA 3 (list
                                      (make-assignment UDON 4)
                                      (make-assignment RAMEN 2))) false)

(check-expect (double-booked? SOBA 3 (list
                                      (make-assignment UDON 4)
                                      (make-assignment SOBA 3)
                                      (make-assignment RAMEN 2)
                                      (make-assignment SOBA 1))) true)
(check-expect (double-booked? SOBA 1 (list
                                      (make-assignment UDON 4)
                                      (make-assignment SOBA 3)
                                      (make-assignment RAMEN 2)
                                      (make-assignment SOBA 1))) true)


(check-expect (double-booked? UDON 3 (list
                                      (make-assignment UDON 4)
                                      (make-assignment SOBA 3)
                                      (make-assignment RAMEN 2)
                                      (make-assignment SOBA 1))) false)

(check-expect (double-booked? UDON 4 (list
                                      (make-assignment UDON 4)
                                      (make-assignment SOBA 3)
                                      (make-assignment RAMEN 2)
                                      (make-assignment SOBA 1))) true)
(check-expect (double-booked? UDON 3 (list
                                      
                                      (make-assignment SOBA 3)
                                      (make-assignment UDON 4)
                                      (make-assignment RAMEN 2)
                                      (make-assignment SOBA 1))) false)

(check-expect (double-booked? UDON 4 (list
                                      
                                      (make-assignment SOBA 3)
                                      (make-assignment UDON 4)
                                      (make-assignment RAMEN 2)
                                      (make-assignment SOBA 1))) true)
(check-expect (double-booked? UDON 3 (list
                                      
                                      (make-assignment SOBA 3)
                                      (make-assignment RAMEN 2)
                                      (make-assignment UDON 4)
                                      (make-assignment SOBA 1))) false)

(check-expect (double-booked? UDON 4 (list
                                      
                                      (make-assignment SOBA 3)
                                      (make-assignment RAMEN 2)
                                      (make-assignment UDON 4)
                                      (make-assignment SOBA 1))) true)


(check-expect (double-booked? UDON 3 (list
                                      
                                      (make-assignment SOBA 3)
                                      (make-assignment RAMEN 2)
                                      (make-assignment SOBA 1)
                                      (make-assignment UDON 4))) false)

(check-expect (double-booked? UDON 4 (list
                                      
                                      (make-assignment SOBA 3)
                                      (make-assignment RAMEN 2)
                                      (make-assignment SOBA 1)
                                      (make-assignment UDON 4))) true)


(define (double-booked? ta slot rsf)
  (local [
          (define (double-booked? ta slot rsf)
            (cond [(empty? rsf) false]
                  [else (if (equal? ta (assignment-ta (first rsf)))
                            (if (equal? (assignment-slot (first rsf)) slot)
                                true
                                (double-booked? ta slot (rest rsf)))
                            (double-booked? ta slot (rest rsf)))]))]
    
    
    (double-booked? ta slot rsf)))


;; TA Slot (listof TA) -> (listof TA)
;; removes Slot from TA's avail list, decrements TA's max
;(check-expect (update-tas SOBA 1 NOODLE-TAs) (list (make-ta "Soba" 1 (list 3)) UDON RAMEN))
;(check-expect (update-tas UDON 4 NOODLE-TAs) (list SOBA (make-ta "Udon" 0 (list 3)) RAMEN))
;(check-expect (update-tas RAMEN 2 NOODLE-TAs) (list SOBA UDON (make-ta "Ramen" 0 empty)))

(define (update-tas ta slot tas) empty)
#;
(define (update-tas ta slot tas)
  (cond [(empty? tas) empty]
        [else (if (equal? ta (first tas))
                  (cons (make-ta (ta-name ta) (sub1 (ta-max ta)) (delete slot (ta-avail ta))) (rest tas))
                  (cons (first tas) (update-tas ta slot (rest tas))))]))


