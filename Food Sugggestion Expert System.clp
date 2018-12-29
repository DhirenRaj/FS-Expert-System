;----* INTRODUCTION *----
;This expert system is name burgerfi food suggesting system.
;our expert system is food suggesting system based on the burgerfi menu.
;This system suggests food to user by taking their food habits as input.
;the questions this system asks the user are "how hungry is the user" , "does he like to experience different tastes or not",
;does the user likes raw vegetables or not", does the user like cheesy items or not", and so on.
;The suggested food may contains burger, dog , drink based on the answers given by the user.
;The type of burger, dog, drink also determined by the answers given by the user.


;----* STEPS TO RUN *----
;open clips and click file and then click load
;now select the file to load
;now click execution and click reset
;now click execution and click run
;then the program will start running


;----* This function is used to ask questions which has other than yes or no answers 
;      This function asks the same question until the user gives the allowed values as answer *----

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
   

;----* This function is used to ask questions which has  yes or no answers 
;      This function asks the same question until the user gives the allowed values(yes or no) as answer *----
   
   (deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))
	
	
;----* person is a classs which has customer preferences as attributes *----
	   
(defclass person (is-a USER) 
	(slot hungry)
	(slot likes_cheese)
	(slot likes_vegetables)
	(slot likes_different_items)
	(slot likes_different_tastes)
	(slot likes_spicy_items)
	(slot likes_mustard)
	(slot drink)
	(slot likes_custard)
	(slot find)
	)
	
	
;----* burger is a classs which has burger characteristics as attributes *----

(defclass burger (is-a USER)
    ;(slot name)
	(slot type)
	(slot has_cheese)
	(slot has_lettuce)
	(slot has_firesauce)
	(slot has_tomato)
	(slot no_of_pattys)
	;(message-handler assert_type_burgerfi)
	;(message-handler assert_type_normal)
	)
	
	
;----* suggested is a classs which has suggested food characteristics as attributes *----	
	
(defclass suggested (is-a USER)
    (slot burger_type)
	(slot pattys)
	(slot cheese)
	(slot has_dog)
	(slot dog_type)
	(slot has_spicy)
	(slot has_mustard)
	(slot has_drink)
	(slot drink_type)
	(slot find)
)


;----* dog is a classs which has dog characteristics as attributes *----

(defclass dog (is-a USER)
    ;(slot name)
    (slot type)
	(slot has_spicy)
	(slot has_mustard)

)


	;(defmessage-handler burger assert_type_burgerfi () (bind ?self:type burgerfi))
;(defmessage-handler burger assert_type_normal () (bind ?self:type normal) )


;----* creating objects for burger class each object defines each burger *----

(definstances BURGER_INSTANCES
(single_burgerfi_burger of burger (has_cheese no) (has_lettuce yes) (has_firesauce yes) (has_tomato yes) (no_of_pattys 1)(type burgerfi))
(double_burgerfi_burger of burger (has_cheese no) (has_lettuce yes) (has_firesauce yes) (has_tomato yes) (no_of_pattys 2)(type burgerfi))
(single_burgerfi_cheese_burger of burger (has_cheese yes) (has_lettuce yes) (has_firesauce yes) (has_tomato yes) (no_of_pattys 1)(type burgerfi))
(double_burgerfi_cheese_burger of burger (has_cheese yes) (has_lettuce yes) (has_firesauce yes) (has_tomato yes) (no_of_pattys 2)(type burgerfi))
(single_burger of burger (has_cheese no) (has_lettuce no) (has_firesauce no) (has_tomato no) (no_of_pattys 1)(type normal))
(double_burger of burger (has_cheese no) (has_lettuce no) (has_firesauce no) (has_tomato no) (no_of_pattys 2)(type normal))
(single_cheese_burger of burger (has_cheese yes) (has_lettuce no) (has_firesauce no) (has_tomato no) (no_of_pattys 1)(type normal))
(double_cheese_burger of burger (has_cheese yes) (has_lettuce no) (has_firesauce no) (has_tomato no) (no_of_pattys 2)(type normal))
)


;----* creating objects for suggested class *----

(definstances SUGGESTED_INSTANCES
(suggest of suggested (burger_type)(pattys)(cheese)(has_dog)(dog_type)(has_spicy)(has_mustard)(has_drink)(drink_type)(find true))
)


;----* creating objects for person class *----

(definstances PERSON_INSTANCES
(preference of person (hungry)(likes_cheese)(likes_vegetables)(likes_different_items)(likes_different_tastes)(likes_spicy_items)(likes_mustard)(drink)(likes_custard)(find true)))


;----* creating objects for dog class each object defines each dog *----

(definstances DOG_INSTANCES
(plain_dog of dog (type normal)(has_spicy no)(has_mustard no))
(newyork_dog of dog (type special)(has_spicy no)(has_mustard yes))
(texas_dog of dog (type special)(has_spicy yes)(has_mustard no))
)


;----* This rule prints the welcome statement to user *----

(defrule welcome
(declare (salience 10))
	
	
	=> 
	(printout t crlf crlf"----* welcome to burgerfi food suggesting expert system *----" crlf crlf)
	)


;----* This rule asks the user how hungry the user is *----

(defrule hungry
	?x<-(object (is-a person ))
	
	=> (send ?x put-hungry (ask-question "are you hungry(little/medium/heavy)" little medium heavy))
	;(printout t (send ?x get-hungry) crlf)
	)
	

;----* This rule rule suggest number of pattys in burger is 2 and adds dog to food when the user enter heavy for hungry *----	

(defrule suggest_pattys_heavy
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	=> (if  (= (str-compare (send ?x get-hungry) "heavy") 0)
                
             then (send ?y put-pattys 2)
			 (send ?y put-has_dog yes)
	;(printout t (send ?y get-pattys) (send ?y get-has_dog) crlf) 
	)
        ) 
		
		
;----* This rule rule suggest number of pattys in burger is 1 when the user enter little for hungry *----

(defrule suggest_pattys_little
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	=> (if  (= (str-compare (send ?x get-hungry) "little") 0)
                
             then (send ?y put-pattys 1)
			 
	;(printout t (send ?y get-pattys) crlf)
	)
        ) 
	
	
;----* This rule ask the user whether the user likes to experience different tastes or not *----
	
(defrule likes_different_tastes
	?x<-(object (is-a person ))
	
	=> (send ?x put-likes_different_tastes (ask-question "did you like to experience different tastes(yes/no)" yes no))
	;(printout t (send ?x get-likes_different_tastes) crlf)
	)
	

;----* This rule ask the user whether the user likes raw vegetables or not *----

(defrule likes_vegetables
	?x<-(object (is-a person ))
	
	=>(if  (= (str-compare (send ?x get-likes_different_tastes) "yes") 0)
                
             then (send ?x put-likes_vegetables (ask-question "did you like raw vegetables(yes/no)" yes no))
	;(printout t (send ?x get-likes_vegetables) crlf) 
	)
        )
		
		
;----* This rule sets the type of burger to burgerfi burger when the user likes to experience different tastes and likes to eat raw vegetables otherwise set to normal *----
		
(defrule suggest_burger_type
(declare(salience -10))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	=> (if  (and (= (str-compare (send ?x get-likes_different_tastes) "yes") 0) (= (str-compare (send ?x get-likes_vegetables) "yes") 0))
                
             then (send ?y put-burger_type burgerfi)
			 
	;(printout t (send ?y get-burger_type) crlf)
else (send ?y put-burger_type normal) 
;(printout t (send ?y get-burger_type) crlf)	
)
        ) 
	
	
;----* This rule ask the user whether the user likes cheesy stuff or not *----

(defrule likes_cheese
	?x<-(object (is-a person ))
	
	=> (send ?x put-likes_cheese (ask-question "did you like cheesy stuff(yes/no)" yes no))
	;(printout t (send ?x get-likes_cheese) crlf)
	)
	
	
;----* This rule suggets cheese in food when the user likes cheesy stuff *----
	
(defrule suggest_cheese
(declare(salience -10))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	=> (if  (= (str-compare (send ?x get-likes_cheese) "yes") 0) 
                
             then (send ?y put-cheese yes)
			 
	;(printout t (send ?y get-cheese) crlf)
else (send ?y put-cheese no) 
;(printout t (send ?y get-cheese) crlf)
	)
        ) 	


;----* This rule ask the user whether the user likes to taste different items or not *----

(defrule likes_different_items
	?x<-(object (is-a person ))
	
	=>(if   (= (str-compare (send ?x get-hungry) "medium") 0)
                
             then (send ?x put-likes_different_items (ask-question "did you like to eat more than one item(yes/no)" yes no))
	;(printout t (send ?x get-likes_different_items) crlf) 
	)
        )
		
		
;----* This rule sets number of pattys in burger to 1 and add dog to food when user gives medium to hungry and the user likes to taste different items *----

(defrule suggest_pattys_medium1
(declare(salience -10))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	=> (if  (= (str-compare (send ?x get-likes_different_items) "yes") 0) 
                
             then (send ?y put-pattys 1) (send ?y put-has_dog yes)
			 
	;(printout t (send ?y get-pattys) (send ?y get-has_dog) crlf)

        )
		)
		
		
;----* This rule sets number of pattys in burger to 2 when user gives medium to hungry and the user didn't like to taste different items *----

(defrule suggest_pattys_medium2
(declare(salience -10))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	=> (if  (= (str-compare (send ?x get-likes_different_items) "no") 0) 
                
             then (send ?y put-pattys 2) (send ?y put-has_dog no)
			 
	;(printout t (send ?y get-pattys) (send ?y get-has_dog) crlf)

        )
		)
		
		
;----* This rule sets dog type to special when user the user likes to experience different tastes and also dog is added to suggested food *----

(defrule suggest_dog_type_special
(declare (salience -20))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if   (and (= (str-compare (send ?x get-likes_different_tastes) "yes") 0) (= (str-compare (send ?y get-has_dog) "yes") 0) )
                
             then (send ?y put-dog_type special)
	;(printout t (send ?y get-dog_type) crlf)
	)
        )
		

;----* This rule sets dog type to normal when the user din't like to experience different tastes and the dog is added to suggested food *----

(defrule suggest_dog_type_normal
(declare (salience -20))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if   (and (= (str-compare (send ?x get-likes_different_tastes) "no") 0) (= (str-compare (send ?y get-has_dog) "yes") 0) )
                
             then (send ?y put-dog_type normal)(send ?y put-has_spicy no)(send ?y put-has_mustard no)
	;(printout t (send ?y get-dog_type)(send ?y get-has_spicy)(send ?y get-has_mustard) crlf) 
	)
        )
		

;----* This rule ask the user whether the user likes spicy items or not only when the dog type is set to special *----
	
(defrule likes_spicy_items
(declare (salience -30))
?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if    (= (str-compare (send ?y get-dog_type) "special") 0)
                
             then (send ?x put-likes_spicy_items (ask-question "did you like to eat spicy item(yes/no)" yes no))
	;(printout t (send ?x get-likes_spicy_items) crlf) 
	)
        ) 
		

;----* This rule adds spicy items to suggested food when the user yes to spicy items *----

(defrule suggest_spicy_yes
(declare (salience -40))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if    (= (str-compare (send ?x get-likes_spicy_items) "yes") 0) 
                
             then (send ?y put-has_spicy yes)(send ?y put-has_mustard no)
	;(printout t (send ?y get-has_spicy)(send ?y get-has_mustard) crlf) 
	)
        )
	
		
;----* This rule didn't add spicy items in food when the user gives no to spicy items *----

(defrule suggest_spicy_no
(declare (salience -40))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if    (= (str-compare (send ?x get-likes_spicy_items) "no") 0) 
                
             then (send ?y put-has_spicy no)
	;(printout t (send ?y get-has_spicy) crlf) 
	)
        )
		

;----* This rule ask the user whether the user likes mustard or not only when the user says no to spicy items *----
		
(defrule likes_mustard
(declare (salience -50))
?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if   (and (= (str-compare (send ?y get-has_spicy) "no") 0) (= (str-compare (send ?y get-dog_type) "special") 0))
                
             then (send ?x put-likes_mustard (ask-question "did you like mustard(yes/no)" yes no))
	;(printout t (send ?x get-likes_mustard) crlf) 
	)
        )
		
		
;----* This rule adds mustard to food when the user says yes to mustard *----
		
(defrule suggest_mustard_yes
(declare (salience -60))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if    (= (str-compare (send ?x get-likes_mustard) "yes") 0) 
                
             then (send ?y put-has_mustard yes)
	;(printout t (send ?y get-has_mustard) crlf) 
	)
        )
		
	
;----* This rule didn't adds mustard to food when the user says no to mustard *----

(defrule suggest_mustard_no
(declare (salience -60))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if    (= (str-compare (send ?x get-likes_mustard) "no") 0) 
                
             then (send ?y put-has_mustard no)
	;(printout t (send ?y get-has_mustard) crlf) 
	)
        )
		

;----* This rule resets the dog type from special to normal when the user says no to both spicy items and mustard *----
		
(defrule suggest_dog_type
(declare (salience -70))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if  (and  (= (str-compare (send ?y get-has_mustard) "no") 0) (= (str-compare (send ?y get-has_spicy) "no") 0))
                
             then (send ?y put-dog_type normal)
	;(printout t (send ?y get-dog_type) crlf) 
	)
        )
		

;----* This rule aska the user whether the user needs something to drink or not *----
		
(defrule prefered_drink
(declare (salience -80))
	?x<-(object (is-a person ))
	
	=> (send ?x put-drink (ask-question "do you want something to drink(yes/no)" yes no))
	;(printout t (send ?x get-hungry) crlf)
	)
	
	
;----* This rule adds drink to food when the user says yes to drink *----

(defrule drink
(declare (salience -90))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if    (= (str-compare (send ?x get-drink) "yes") 0) 
                
             then (send ?y put-has_drink yes)
	;(printout t (send ?y get-has_mustard) crlf) 
	else (send ?y put-has_drink no)
	)
        )
		
		
;----* This rule asks the user whether the user likes custards, icecreams or not *----

(defrule prefered_custard
(declare (salience -100))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if    (= (str-compare (send ?y get-has_drink) "yes") 0) 
                
             then (send ?x put-likes_custard (ask-question "do you like icecream or custards (yes/no)" yes no))
	;(printout t (send ?y get-has_mustard) crlf) 
	;else (send ?y put-has_drink no)
	)
        )
		
		
;----* This rule sets drink type in suggested food to milkshake when the user says yes to custards or icecreams otherwise sets to soda *----

(defrule suggested_drink_type
(declare (salience -110))
	?x<-(object (is-a person ))
	?y<-(object (is-a suggested))
	
	=>(if    (= (str-compare (send ?x get-likes_custard) "yes") 0) 
                
             then (send ?y put-drink_type milkshake)
	;(printout t (send ?y get-has_mustard) crlf) 
	else (send ?y put-drink_type soda)
	)
        )		

		
;----* This rule suggets the name of burger by using the characteristics set to suggested food by matching them with burger objects and print it to user *----

(defrule suggested_burger
(declare (salience -120))
	?x<-(object (is-a suggested )(burger_type ?a)(pattys ?b)(cheese ?c)(find true))
	?y<-(object (is-a burger)(name ?d)(type ?a)(has_cheese ?c)(no_of_pattys ?b))
	
	
	=> 
	(printout t crlf crlf"food suggested is " ?d crlf)
	)


;----* This rule suggets the name of dog by using the characteristics set to suggested food by matching them with dog objects and print it to user when dog is added to food *----

(defrule suggested_dog
(declare (salience -140))
	?x<-(object (is-a suggested )(dog_type ?a)(has_spicy ?b)(has_mustard ?c)(has_dog yes))
	?y<-(object (is-a dog)(name ?d)(type ?a)(has_mustard ?c)(has_spicy ?b))
	
	
	=> 
	(printout t " and " ?d crlf)
	)
	
	
;----* This rule prints the type of drink suggested to user only when the drink is added to food *----

(defrule suggested_drink
(declare (salience -150))
	?x<-(object (is-a suggested) (drink_type ?a)(has_drink yes))
	;?y<-(object (is-a dog)(name ?d)(type ?a)(has_mustard ?c)(has_spicy ?b))
	
	
	=> 
	(printout t " with " ?a crlf crlf crlf)
	)