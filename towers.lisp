;; Name: Andrew McDonald
;; Student#: 17961040
;; Date: 31.08.18

;;;; =====================================================
;;;; COMMON LISP IMPLEMENTATION OF TOWERS OF HANOI PROBLEM
;;;; =====================================================

;;; =====================
;;; STATE REPRESENTATIION
;;; =====================

;; A state is represented as a list of three sub-lists, each of which can take
;; the values representing discs in successive order (1,2,3,etc).  The elements of the 
;; list represent the discs, and the list themselves represent the towers.

;;; ===========================
;;; STATE CONSTRUCTOR FUNCTIONS
;;; ===========================

;; TOWER-DISC-SIZE takes a number (discs) as an input and returns a list of that size 
;; starting from 1 to (disc) this returns a list representing the first tower (A) with 
;; n discs attached
(defun make-problem-size (discs)
  (let (tower-size)
    (dotimes (n discs (reverse tower-size))
      (setq tower-size (cons (+ n 1) tower-size)))))

;; MAKE-STATE takes three input parameters, and returns thems as a list of three lists
(defun make-state (a b c)
  (list a b c))

;; Define *size* as a global.
;; sets the amount of discs for tower of hanoi problem
(defparameter *size* 3)
 
 
;;; ====================================================
;;; START STATE, GOAL STATE, AND TEST FOR SOLUTION STATE
;;; ====================================================

;; Define *start-state* as a global.
;; (Required by breadth-first-search)
(defparameter *start-state*
  (make-state (make-problem-size *size*) () ()))

;; Define *goal-state* as a global.
(defparameter *goal-state*
  (make-state () () (make-problem-size *size*)))

;; takes a state as input and returns t if the
;; state is a solution to the problem.
(defun solution-state? (state)
  (if (equalp state *goal-state*) T))


;;; ========================
;;; STATE ACCESSOR FUNCTIONS
;;; ========================

;; FIND-A-TOWER takes a state as input and returns the first list (tower).
(defun find-A-tower (state)
  (nth 0 state))

;; FIND-B-TOWER takes a state as input and returns the second list (tower).
(defun find-B-tower (state)
  (nth 1 state))

;; FIND-C-TOWER takes a state as input and returns the third list (tower).
(defun find-C-tower (state)
  (nth 2 state))

;; TOP-DISC takes a list as input (tower) and returns the 
;; first element (top disc).
(defun top-disc (tower)
  (nth 0 tower))


;;; =================
;;; PROBLEM OPERATORS
;;; =================

;; *OPERATORS* is a global list containing the names of the six operators
(defparameter *operators* '(move-A-to-B 
							move-A-to-C 
							move-B-to-A 
							move-B-to-C 
							move-C-to-A 
							move-C-to-B))

;; MOVE-A-TO-B takes a state as input, checks if it's legal to move the top disc from 
;; tower A to tower B, and if so returns new legal state, otherwise nil is returned.
(defun 	move-A-to-B (state)
  (if (legal-move? (find-A-tower state) 
                   (find-B-tower state))
	  (make-state (remove-from (find-A-tower state)) 
	  			  (add-to (find-A-tower state)(find-B-tower state)) 
	  			  (find-C-tower state)) 
	   nil))


;; MOVE-A-TO-C takes a state as input, checks if it's legal to move the top disc from 
;; tower A to tower C, and if so returns new legal state, otherwise nil is returned.
(defun 	move-A-to-C (state)
  (if (legal-move? (find-A-tower state) 
				   (find-C-tower state))	
	  (make-state (remove-from (find-A-tower state)) 
				  (find-B-tower state) 
				  (add-to (find-A-tower state)(find-C-tower state))) 
	   nil))

;; MOVE-B-TO-A takes a state as input, checks if it's legal to move the top disc from 
;; tower B to tower A, and if so returns new legal state, otherwise nil is returned.
(defun 	move-B-to-A (state)
  (if (legal-move? (find-B-tower state) 
				   (find-A-tower state))	
	  (make-state (add-to (find-B-tower state)(find-A-tower state)) 
				  (remove-from (find-B-tower state))
				  (find-C-tower state)) 
	   nil))

;; MOVE-B-TO-C takes a state as input, checks if it's legal to move the top disc from 
;; tower B to tower C, and if so returns new legal state, otherwise nil is returned.
(defun 	move-B-to-C (state)
  (if (legal-move? (find-B-tower state) 
                   (find-C-tower state))	
	  (make-state (find-A-tower state) 
				  (remove-from (find-B-tower state)) 
				  (add-to (find-B-tower state)(find-C-tower state))) 
	   nil))

;; MOVE-C-TO-A takes a state as input, checks if it's legal to move the top disc from 
;; tower C to tower A, and if so returns new legal state, otherwise nil is returned.
(defun 	move-C-to-A (state)
  (if (legal-move? (find-C-tower state) 
				   (find-A-tower state))	
      (make-state (add-to (find-C-tower state)(find-A-tower state)) 
	              (find-B-tower state)
			      (remove-from (find-C-tower state))) 
	   nil))

;; MOVE-C-TO-B takes a state as input, checks if it's legal to move the top disc from 
;; tower C to tower B, and if so returns new legal state, otherwise nil is returned.
(defun 	move-C-to-B (state)
  (if (legal-move? (find-C-tower state) 
				   (find-B-tower state))	
	  (make-state (find-A-tower state) 
				  (add-to (find-C-tower state)(find-B-tower state)) 
				  (remove-from (find-C-tower state))) 
	   nil))


;;; ================
;;; HELPER FUNCTIONS
;;; ================

;; LEGAL-MOVE? takes in two lists (towers) as inputs, representing a 'from tower' 
;; and a 'to tower'. Conditionally checks for legal-state of move and returns
;; true if legal, nil if non-legal. 
;; Rules: (1) from tower cannot be empty. 
;;        (2) to tower can be empty.
;;        (3) top disc in from tower must be less-than the top disc in to tower.
(defun legal-move? (from to)
  (cond ((null from) nil)
        ((null to) T) 
		((< (top-disc from) (top-disc to)) T)))

;; REMOVE-FROM takes one input, a list (tower) and removes the first element 
;; from the list, returning the new list.
(defun remove-from (tower)
  (cdr tower))

;; ADD-TO takes two inputs, a 'from list' and a 'to list' (towers) and adds 
;; the first element from the 'from list' to the 'to list', returning the 
;; 'to list'.
(defun add-to (from to)
  (cons (car from) to))


;;;; ==========================
;;;; HEURISTIC SEARCH FUNCTIONS
;;;; ==========================

;;; ====================
;;; EVALUATION FUNCTIONS
;;; ====================

;; COST-OF-APPLYING-OPERATOR takes a state and an operator as input parameters, 
;; and returns the cost of applying the operator to that state. We assume that all 
;; costs are equal, and hence this function always returns 1, irrespective of the 
;; state or the operator.
(defun cost-of-applying-operator (state operator) 1)

;; ESTIMATED-DISTANCE-FROM-GOAL takes a state as a parameter, and returns an estimate 
;; of the number of steps required to get from this state to the goal by adding the 
;; values returned by functions order-of-tower?, where C tower is input, and 
;; length-of-tower?, where A tower is input. Returns 0 for success.
;;  
;; Function order-of-tower? is weighted so that a priority is given to the next 
;; smallest disc in order. 
;;
;; Function length-of-tower is weighted based on the symetry that exists between 
;; start-state and goal-state; where if A tower is full, C tower is empty and vice versa.
(defun estimated-distance-from-goal (state)
  ; returned values are a list of fractions
  (+  (float (nth 0 (reverse (order-of-tower? (find-C-tower state) *weight-start-value1*))))
	  (float (length-of-tower? (find-A-tower state) *weight-start-value2*))))
	  
	  
;;; =================================
;;; WEIGHT FACTOR AND STARTING VALUES
;;; =================================

;; Define *weight-factor* as a global.
;; sets the weight factor for the heuristic functions.
(defparameter *weight-factor* 3) ; on testing, 3 was most effective

;; Define *weight-start-value1* as a global.
;; sets the weight value for the order-of-tower? heuristic function.
;; On testing f(x) = x^(*size*), where x is *weight-factor* was most effective.
(defparameter *weight-start-value1* (expt *weight-factor* *size*))

;; Define *weight-start-value2* as a global.
;; sets the weight value for the length-of-tower? heuristic function.
;; On testing f(x) = (*size*)^x, where x is *weight-factor* was most effective.
(defparameter *weight-start-value2* (expt *size* *weight-factor*))


;;; ===================
;;; ALGORITHM FUNCTIONS
;;; ===================

;; ORDER-OF-TOWER? takes a tower as input and checks the the order of the discs and 
;; returns a value depending on the order. Returned value num starts at number of discs 
;; of problem raised to the order of 3, which decrements by a factor of 3 for every disc 
;; that is in its correct place and order. Ultimately, returning 0 if all discs are on C 
;; tower and in correct order.
(defun order-of-tower? (tower s-value)
  (let ((num s-value)
		(b *size*) ; set to largest disc
		(count 0)) ; count is incremented throughout loop, used to check for final smallest disc 
    (loop for x from 0 to *size* collect num
	  if (equalp (nth x (reverse tower)) b) ; if disc is in correct spot
	    do (decf num (/ num *size*))
	  (decf b)            ; move to next smallest disc size
	  (incf count)        ; increment count
	  if (= count *size*) ; if final correct disc is in place, reduce num to zero.
	    do (decf num num))))
		
;; LENGTH-OF-TOWER takes a tower as an input and recursively tracks through tower list, 
;; adding s-value divided by *weight-factor* to the returned value for every element found. 
;; If no elements are found, 0 is returned. If all elements up to *size* are found, full 
;; s-value is returned.	 
(defun length-of-tower? (tower s-value)
    (if (< (length tower) 1) 0
        (+ (/ s-value *weight-factor*) 
		   (length-of-tower? (cdr tower) (/ s-value *weight-factor*)))))		 