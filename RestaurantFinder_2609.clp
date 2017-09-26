; http://sequoia.ict.pwr.wroc.pl/~witold/ai/CLIPS_tutorial/// 
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
  (assert (value ?field ?def)))

(defrule check-boolean-true-true
  ?fv  <- (value ?field ?s&:(eq ?s yes))
  ?f <- (get ?field)
  ?fc <- (check boolean ?field ?def)
  =>
  (retract ?f ?fc ?fv)
  (assert (value ?field True)))

(defrule check-boolean-true-false
  ?fv  <- (value ?field ?s&:(eq ?s no))
  ?f <- (get ?field)
  ?fc <- (check boolean ?field ?def)
  =>
  (retract ?f ?fc ?fv)
  (assert (value ?field False)))

(defrule check-boolean-false
  ?fs <- (value ?field ?s&:(and (neq ?s yes) (neq ?s no)))
  ?f <- (get ?field)
  ?fc <- (check boolean ?field ?def)
  =>
  (retract ?fs ?f ?fc)
  (assert (get ?field))
  (printout t "The input must be yes or no" crlf))

(defrule check-integer-true
  (value ?field ?mb&:(and (integerp ?mb) (>= ?mb 0) (<= ?mb 9999)))
  ?f <- (get ?field)
  ?fc <- (check integer ?field ?def)
  =>
  (retract ?f ?fc))

(defrule check-integer-false
  ?fmb <- (value ?field ?mb&:(or (not (integerp ?mb)) (< ?mb 0) (> ?mb 9999)))                                                                                                                 
  ?f <- (get ?field)
  ?fc <- (check integer ?field ?def)
  =>
  (retract ?f ?fmb ?fc)
  (assert (get ?field))
  (printout t "The input must be an integer between 0 and 9999" crlf))


(defrule check-clothes-true
  (value clothes ?c&:(or (eq ?c formal) (eq ?c casual) (eq ?c informal)))
  ?f <- (get clothes)
  ?fc <- (check whatever clothes ?def)
  =>
  (retract ?f ?fc))

(defrule check-clothes-false
  ?fcl <- (value clothes ?c&:(and (neq ?c casual) (neq ?c informal) (neq ?c formal)))
  ?f <- (get clothes)
  ?fc <- (check whatever clothes ?def)
  =>
  (retract ?fcl ?f ?fc)
  (assert (get clothes))
  (printout t "The input must be casual, informal, or formal" crlf))

(defrule check-float-true
  ?fl <- (value ?field ?lt&:(numberp ?lt))
  ?f <- (get ?field)
  ?fc <- (check float ?field ?def)
  =>
  (retract ?f ?fc ?fl)
  (assert (value ?field (float ?lt))))

(defrule check-float-false
  ?fl <- (value ?field ?lt&:(not (floatp ?lt)))
  ?f <- (get ?field)
  ?fc <- (check float ?field ?def)
  =>
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
    =>
    (do-for-all-facts ((?f restaurant)) TRUE
        (bind ?count 0)
        (if (eq ?f:isSmoker ?is)
            then
            (bind ?count (+ ?count 1))
        )
        (if (and (>= ?mib ?f:minimumBudget) (>= ?mab ?f:maximumBudget))
            then
            (bind ?count (+ ?count 1))
        )
        (if (eq ?f:hasWifi ?wf)
            then
            (bind ?count (+ ?count 1))
        )
        (foreach ?field $?f:dressCode 
            (if (eq ?field ?cl)
                then
                (bind ?count (+ ?count 1)))
        )
        (assert (restaurantValue (name ?f:name) (numOfMatch ?count) (category (set-category ?count)) (distance (distance-lat-long ?lat ?long ?f:latitude ?f:longitude))))
    )
    (assert (solution 1))
)

(deffunction is-min-dist (?name ?dis ?cat)
  (bind ?min TRUE)
  (do-for-all-facts ((?val restaurantValue)) TRUE
    (if (and (= ?val:category ?cat) (> ?dis ?val:distance))
      then
      (bind ?min FALSE)))
  (return ?min))

(defrule print-solution
  =>
  (printout t crlf "Here's our recommendation: " crlf))

(defrule pick-1-distance
  (declare (salience -5))
  ?fc <- (solution ?n&:(<= ?n 3))
  ?fres <- (restaurantValue (category 1) (name ?na) (distance ?dis))
  (test (is-min-dist ?na ?dis 1))
  =>
  (retract ?fc ?fres)
  (assert (solution (+ ?n 1)))
  (printout t ?n ". Restaurant " ?na ": Very recommendable (distance: " ?dis ")" crlf))

(defrule pick-2-distance
  (declare (salience -6))
  ?fc <- (solution ?n&:(<= ?n 3))
  ?fres <- (restaurantValue (category 2) (name ?na) (distance ?dis))
  (test (is-min-dist ?na ?dis 2))
  =>
  (retract ?fc ?fres)
  (assert (solution (+ ?n 1)))
  (printout t ?n ". Restaurant " ?na ": Recommendable (distance: " ?dis ")" crlf))
  
(defrule pick-3-distance
  (declare (salience -7))
  ?fc <- (solution ?n&:(<= ?n 3))
  ?fres <- (restaurantValue (category 3) (name ?na) (distance ?dis))
  (test (is-min-dist ?na ?dis 3))
  =>
  (retract ?fc ?fres)
  (assert (solution (+ ?n 3)))
  (printout t ?n ". Restaurant " ?na ": Not recommendable (distance: " ?dis ")" crlf))
  
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
