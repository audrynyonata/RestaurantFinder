; http://sequoia.ict.pwr.wroc.pl/~witold/ai/CLIPS_tutorial/// 
; https://github.com/audrynyonata/RestaurantFinder/
; https://docs.google.com/document/d/1JxjQBFD894YE9shWa64igvP5fuIvggRKFlK2PtVVS5Q/edit
; collabedit.com/xdwf3
; WARNING CUMA 60 DAYS
; deftemplate 

(deftemplate restaurant
  (field name)
  (field isSmoker)
  (field minimumBudget)
  (field maximumBudget)
  (multislot dressCode)
  (field hasWifi)
  (field latitude)
  (field longitude))
  
(deftemplate restaurantValue
  (field name)
  (field numOfMatch)
  (field category)
  (field distance))
  ;category : 1 for very recommended, 2 recommended, 3 not recommended
  
(deftemplate isEmpty
    (field fieldName)
    (field empty))
  
(defrule logo
  (declare (salience 99))
  =>
  (printout t
    "                      ___          /|     " crlf
    "         ||||     .-\"`   `\"-.     } |  __ " crlf
    "    |||| ||||   .'  .-'`'-.  '.   } | /  \\" crlf
    "    |||| \\  /  /  .'       '.  \\  } | ;();" crlf
    "    \\  /  ||  /  ;           ;  \\  \\| \\  /" crlf
    "     ||   ||  | ;             ; |  ||  ||       RESTAURANT FINDER" crlf
    "     %%   %%  | ;             ; |  %%  %%        by Tim Ultron" crlf
    "     %%   %%  \\  ;           ;  /  %%  %% " crlf
    "     %%   %%   \\  '.       .'  /   %%  %% " crlf
    "     %%   %%    '.  `-.,.-'  .'    %%  %% " crlf
    "     %%   %%      '-.,___,.-'      %%  %% " crlf crlf))    

;input
(defrule input-name
  ?f <- (get name)
  =>
  (printout t "What is your name ? ")
  (assert (value name (explode$ (readline))))
  (retract ?f))

(defrule input-smoke
  (get isSmoke)
  =>
  (printout t "Do you smoke? (yes, no) ")
  (assert (value isSmoke (first$ (explode$ (lowcase (readline))))))
  (assert (check boolean isSmoke False)))

(defrule input-min-budget
  (get minimumBudget)
  =>
  (printout t "What is your minimum budget? [0-9999] ")
  (assert (value minimumBudget (first$ (explode$ (readline)))))
  (assert (check integer minimumBudget 0)))

(defrule input-max-budget
  (get maximumBudget)
  =>
  (printout t "What is your maximum budget? [0-9999] ")
  (assert (value maximumBudget (first$ (explode$ (readline)))))
  (assert (check integer maximumBudget 9999)))

(defrule input-clothes
  (get clothes)
  =>
  (printout t "What clothes are you wearing? (casual, informal, formal) " )
  (assert (value clothes (first$ (explode$ (lowcase (readline))))))
  (assert (check whatever clothes casual)))

(defrule input-wifi
  (get needWifi)
  =>
  (printout t "Do you want a restaurant with Wi-Fi? (yes, no) " )
  (assert (value needWifi (first$ (explode$ (lowcase (readline))))))
  (assert (check boolean needWifi True)))

(defrule input-latitude
  (get latitude)
  =>
  (printout t "What are your lat. coordinate? ")
  (assert (value latitude (first$ (explode$ (readline)))))
  (assert (check float latitude 0)))

(defrule input-longitude
  (get longitude)
  =>
  (printout t "What are your long. coordinate? ")
  (assert (value longitude (first$ (explode$ (readline)))))
  (assert (check float longitude 0)))

;check 
(defrule check-empty
  ?fv <- (value ?field)
  ?f <- (get ?field)
  ?fc <- (check ?type ?field ?def)
  =>
  (retract ?fv ?f ?fc)
  (assert (isEmpty (fieldName ?field) (empty 1)))
  (assert (value ?field ?def)))

(defrule check-boolean-true-true
  ?fv  <- (value ?field ?s&:(eq ?s yes))
  ?f <- (get ?field)
  ?fc <- (check boolean ?field ?def)
  =>
  (assert (isEmpty (fieldName ?field) (empty 0)))
  (retract ?f ?fc ?fv)
  (assert (value ?field True)))

(defrule check-boolean-true-false
  ?fv  <- (value ?field ?s&:(eq ?s no))
  ?f <- (get ?field)
  ?fc <- (check boolean ?field ?def)
  =>
  (assert (isEmpty (fieldName ?field) (empty 0)))
  (retract ?f ?fc ?fv)
  (assert (value ?field False)))

(defrule check-boolean-false
  ?fs <- (value ?field ?s&:(and (neq ?s yes) (neq ?s no)))
  ?f <- (get ?field)
  ?fc <- (check boolean ?field ?def)
  =>
  (assert (isEmpty (fieldName ?field) (empty 0)))
  (retract ?fs ?f ?fc)
  (assert (get ?field))
  (printout t "The input must be yes or no" crlf))

(defrule check-integer-true
  (value ?field ?mb&:(and (integerp ?mb) (>= ?mb 0) (<= ?mb 9999)))
  ?f <- (get ?field)
  ?fc <- (check integer ?field ?def)
  =>
  (assert (isEmpty (fieldName ?field) (empty 0)))
  (retract ?f ?fc))

(defrule check-integer-false
  ?fmb <- (value ?field ?mb&:(or (not (integerp ?mb)) (< ?mb 0) (> ?mb 9999)))                                                                                                                 
  ?f <- (get ?field)
  ?fc <- (check integer ?field ?def)
  =>
  (assert (isEmpty (fieldName ?field) (empty 0)))
  (retract ?f ?fmb ?fc)
  (assert (get ?field))
  (printout t "The input must be an integer between 0 and 9999" crlf))


(defrule check-clothes-true
  (value clothes ?c&:(or (eq ?c formal) (eq ?c casual) (eq ?c informal)))
  ?f <- (get clothes)
  ?fc <- (check whatever clothes ?def)
  =>
  (assert (isEmpty (fieldName clothes) (empty 0)))
  (retract ?f ?fc))

(defrule check-clothes-false
  ?fcl <- (value clothes ?c&:(and (neq ?c casual) (neq ?c informal) (neq ?c formal)))
  ?f <- (get clothes)
  ?fc <- (check whatever clothes ?def)
  =>
  (assert (isEmpty (fieldName clothes) (empty 0)))
  (retract ?fcl ?f ?fc)
  (assert (get clothes))
  (printout t "The input must be casual, informal, or formal" crlf))

(defrule check-float-true
  ?fl <- (value ?field ?lt&:(numberp ?lt))
  ?f <- (get ?field)
  ?fc <- (check float ?field ?def)
  =>
  (assert (isEmpty (fieldName ?field) (empty 0)))
  (retract ?f ?fc ?fl)
  (assert (value ?field (float ?lt))))

(defrule check-float-false
  ?fl <- (value ?field ?lt&:(not (floatp ?lt)))
  ?f <- (get ?field)
  ?fc <- (check float ?field ?def)
  =>
  (assert (isEmpty (fieldName ?field) (empty 0)))
  (retract ?f ?fc ?fl)
  (assert (get ?field))
  (printout t "The input must be a float" crlf))
  
;distance
(deffunction distance-lat-long (?lat1 ?lon1 ?lat2 ?lon2)
  (bind ?p 0.017453292519943295)
  (bind ?dlat (- ?lat2 ?lat1))
  (bind ?dlon (- ?lon2 ?lon1))
  (bind ?x1 (/ (cos (* ?dlat ?p)) 2))
  (bind ?x2 (cos (* ?lat1 ?p)))
  (bind ?x3 (cos (* ?lat2 ?p)))
  (bind ?x4 (/ (- 1 (cos (* ?dlon ?p))) 2))
  (bind ?x5 (* (* ?x2 ?x3) ?x4))
  (bind ?x6 (+ (- 0.5 ?x1) ?x5))
  (bind ?x7 (asin (sqrt ?x6)))
  (bind ?dis (* 12742 ?x7))
  ?dis)
 
;category
(deffunction set-category (?count)
  (if (= ?count 4)
    then
    (bind ?ctg 1)
    else
    (if (>= ?count 2)
      then
      (bind ?ctg 2)
      else
      (bind ?ctg 3))))

(defrule countMatch
    (declare (salience -1))
    (value isSmoke ?is)
    (value minimumBudget ?mib)
    (value maximumBudget ?mab)
    (value clothes ?cl)
    (value needWifi ?wf)
    (value latitude ?lat)
    (value longitude ?long)
    (isEmpty (fieldName isSmoke)(empty ?eis))
    (isEmpty (fieldName minimumBudget)(empty ?eminb))
    (isEmpty (fieldName maximumBudget)(empty ?emaxb))
    (isEmpty (fieldName needWifi)(empty ?enw))
    (isEmpty (fieldName clothes)(empty ?ec))
    =>
    (do-for-all-facts ((?f restaurant)) TRUE
        (bind ?count 0)
        (if (or (eq ?f:isSmoker ?is) (eq ?eis 1))
            then
            (bind ?count (+ ?count 1))
        )
        (if (or (and (>= ?mib ?f:minimumBudget) (>= ?mab ?f:maximumBudget)) (eq ?eminb 1) (eq ?emaxb 1))
            then
            (bind ?count (+ ?count 1))
        )
        (if (or (eq ?f:hasWifi ?wf) (eq ?enw 1))
            then
            (bind ?count (+ ?count 1))
        )
        (if (eq ?ec 1)
              then
              (bind ?count (+ ?count 1))
              else
              (foreach ?field $?f:dressCode 
              (if (eq ?field ?cl)
                  then
                  (bind ?count (+ ?count 1)))
                ))
        (assert (restaurantValue (name ?f:name) (numOfMatch ?count) (category (set-category ?count)) (distance (distance-lat-long ?lat ?long ?f:latitude ?f:longitude))))
    )
    (assert (turn 1))
)

(deffunction is-min-dist (?name ?dis ?cat)
  (bind ?min TRUE)
  (do-for-all-facts ((?val restaurantValue)) TRUE
    (if (and (= ?val:category ?cat) (> ?dis ?val:distance))
      then
      (bind ?min FALSE)))
  (return ?min))
  
(defrule start-turn
  (declare (salience -5))
  (turn ?tu&:(<= ?tu 3))
  (restaurant (name ?name))
  (not (solution ?name))
  =>
  (assert (candidate ?name)))
  
(defrule test-category
  (declare (salience -6))
  (candidate ?dummy)
  ?fc <- (candidate ?can&~?dummy)
  (restaurantValue(name ?can) (category ?cat))
  (exists (and (restaurantValue (name ?na) (category ?cat2&:(> ?cat ?cat2))) (candidate ?na)))
   =>
   (retract ?fc))
   
(defrule test-distance
  (declare (salience -7))
  (candidate ?dummy)
  ?fc <- (candidate ?can&~?dummy)
  (restaurantValue(name ?can) (distance ?dis))
  (exists (and (restaurantValue (name ?na) (distance ?dis2&:(> ?dis ?dis2))) (candidate ?na)))
   =>
   (retract ?fc))
   
(defrule test-wifi
  (declare (salience -8))
  (candidate ?dummy)
   ?fc <- (candidate ?can&~?dummy)
  (restaurantValue(name ?can))
  (restaurant (name ?can) (hasWifi False))
  (exists (and (restaurant (name ?na) (hasWifi True)) (candidate ?na)))
   =>
   (retract ?fc))
   
(defrule test-cheap
  (declare (salience -9))
  (candidate ?dummy)
  ?fc <- (candidate ?can&~?dummy)
  (restaurantValue(name ?can))
  (restaurant (name ?can) (minimumBudget ?bud))
  (exists (and (restaurant (name ?na) (minimumBudget ?bud2&:(> ?bud ?bud2))) (candidate ?na)))
   =>
   (retract ?fc))
   
(defrule test-dresscode
  (declare (salience -10))
  (candidate ?dummy)
  ?fc <- (candidate ?can&~?dummy)
  (restaurantValue(name ?can))
  (restaurant (name ?can) (dressCode ?dc&~casual))
  (exists (and (restaurant (name ?na) (dressCode casual)) (candidate ?na)))
   =>
   (retract ?fc))
   
(defrule test-smoke
  (declare (salience -11))
  (candidate ?dummy)
  ?fc <- (candidate ?can&~?dummy)
  (restaurantValue(name ?can))
  (restaurant (name ?can) (isSmoker True))
  (exists (and (restaurant (name ?na) (isSmoker False)) (candidate ?na)))
   =>
   (retract ?fc))
   
(deffunction get-category (?cat)
  (if (= ?cat 1)
    then
    "Very recommendable"
    else
    (if (= ?cat 2)
      then
      "Recommendable"
      else
      "Not recommendable")))
   
(defrule print-turn
  (declare (salience -12))
  ?fca <- (candidate ?can)
  (turn ?tu&:(<= ?tu 3))
  (restaurantValue (name ?can) (category ?cat) (distance ?dis))
  (not (stop))
  =>
  (retract ?fca)
  (assert (solution ?can))
  (assert (stop))
  (printout t ?tu ". Restoran " ?can ": " (get-category ?cat) " (distance: " ?dis ")" crlf))

(defrule delete-all-candidate
  (declare (salience -13))
  ?fc <- (candidate ?can)
  =>
  (retract ?fc))

(defrule finish-turn
  (declare (salience -14))
  ?fs <- (stop)
  ?ft <- (turn ?tu&:(<= ?tu 3))
  =>
  (retract ?fs ?ft)
  (assert (turn (+ ?tu 1))))

(defrule print-solution
  =>
  (printout t crlf "Here's our recommendation: " crlf))
  
(defrule startup
  =>
  ; Welcoming message
  (printout t
    "Welcome to RestaurantFinder" crlf crlf)
  (assert (get longitude))
  (assert (get latitude))
  (assert (get needWifi))
  (assert (get clothes))
  (assert (get maximumBudget))
  (assert (get minimumBudget))
  (assert (get isSmoke))
  (assert (get name))
    
  ; Declaring restaurants
  (assert (restaurant (name A) (isSmoker True) (minimumBudget 1000) (maximumBudget 2000) 
          (dressCode casual) (hasWifi True) (latitude -6.8922186) (longitude 107.5886173))
          
          (restaurant (name B) (isSmoker False) (minimumBudget 1200) (maximumBudget 2500) 
          (dressCode informal) (hasWifi True) (latitude -6.224085) (longitude 106.7859815))
          
          (restaurant (name C) (isSmoker True) (minimumBudget 2000) (maximumBudget 4000) 
          (dressCode formal) (hasWifi False) (latitude -6.2145285) (longitude 106.8642591))
          
          (restaurant (name D) (isSmoker False) (minimumBudget 500) (maximumBudget 1400) 
          (dressCode formal) (hasWifi False) (latitude -6.9005363) (longitude 107.6222191))
          
          (restaurant (name E) (isSmoker True) (minimumBudget 1000) (maximumBudget 2000) 
          (dressCode informal casual) (hasWifi True) (latitude -6.2055617) (longitude 106.8001597))
          
          (restaurant (name F) (isSmoker False) (minimumBudget 2500) (maximumBudget 5000) 
          (dressCode informal) (hasWifi True) (latitude -6.9045679) (longitude 107.6399745))  
                  
          (restaurant (name G) (isSmoker True) (minimumBudget 1300) (maximumBudget 3000) 
          (dressCode casual) (hasWifi True) (latitude -6.1881082) (longitude 106.7844409))
                    
          (restaurant (name H) (isSmoker False) (minimumBudget 400) (maximumBudget 1000) 
          (dressCode informal) (hasWifi False) (latitude -6.9525133) (longitude 107.6052906))
          
          (restaurant (name I) (isSmoker False) (minimumBudget 750) (maximumBudget 2200) 
          (dressCode informal casual) (hasWifi True) (latitude -6.9586985) (longitude 107.7092281))
                  
          (restaurant (name J) (isSmoker False) (minimumBudget 1500) (maximumBudget 2000) 
          (dressCode casual) (hasWifi True) (latitude -6.2769732) (longitude 106.775133))
   )
)  
