(setq ROOMS 
  '((LIVING-ROOM        (NORTH FRONT-STAIRS)
                        (SOUTH DINING-ROOM)
                        (EAST KITCHEN))
    (UPSTAIRS-BEDROOM   (WEST LIBRARY)
                        (SOUTH FRONT-STAIRS))
    (DINING-ROOM        (NORTH LIVING-ROOM)
                        (EAST PANTRY)
                        (WEST DOWNSTAIRS-BEDROOM))
    (KITCHEN            (WEST LIVING-ROOM)
                        (SOUTH PANTRY))
    (PANTRY             (NORTH KITCHEN)
                        (WEST DINING-ROOM))
    (DOWNSTAIRS-BEDROOM (NORTH BACK-STAIRS)
                        (EAST DINING-ROOM))
    (BACK-STAIRS        (SOUTH DOWNSTAIRS-BEDROOM)
                        (NORTH LIBRARY))
    (FRONT-STAIRS       (NORTH UPSTAIRS-BEDROOM)
                        (SOUTH LIVING-ROOM))
    (LIBRARY            (EAST UPSTAIRS-BEDROOM)
                        (SOUTH BACK-STAIRS)))
)

(defun searchfor (temp x)
    (setq mtemp (car temp))
    (if (equal nil mtemp)
        ()
        (if (equal (car mtemp) x)
            (cdr mtemp)
            (searchfor (cdr temp) x)
        )
    )
)

;A function CHOICES which takes the name of the room and
;   returns the table of permissible choices for Robbie's next
;   destination.
(defun choices (x)
    (searchfor rooms x)
)

;An expression which sets a global variable LOC to hold
;   Robbie's current position in the PANTRY.
(defun setLocation (x)
    (setf loc x)
)

(setlocation 'pantry)

;A predicate function UPSTAIRSP that returns T if it’s input
;   is an upstairs locations
(defun upstairsp (mLoc)
    (cond 
        ((equal 'UPSTAIRS-BEDROOM mLoc))
        ((equal 'LIBRARY mLoc))
    )
)

;A predicate function ONSTAIRSP which returns T if it’s input
;   is either FRONT-STAIRS or BACK-STAIRS.
(defun onstairsp (mLoc)
    (cond 
        ((equal 'FRONT-STAIRS mLoc))
        ((equal 'BACK-STAIRS mLoc))
    )
)

;A function WHERE that requires no inputs and tells Robbie
;   where he is.
(defun where()
    (cond
        ((upstairsp loc) (append (list 'ROBBIE 'IS 'UPSTAIRS 'IN 'THE) (list loc)))
        ((onstairsp loc)  (append (list 'ROBBIE 'IS 'ON 'THE) (list loc)))
        (t (append (list 'ROBBIE 'IS 'DOWNSTAIRS 'IN 'THE) (list loc)))
    )
)

(defun runtwo (dir)
    (setlocation dir)
    (where)
)

;A function MOVE that takes one input, a direction, and moves
;   Robbie in that direction.
(defun move (mDir)
    (setq mLook (look mDir loc))
    (cond
        ((equal nil mLook) (list 'ouch! 'robbie 'hit 'a 'wall))
        (t (runtwo mLook))
    )
)

(defun count-for (mlist mcount)
    (if (equal nil mlist)
        mcount
        (count-for (cdr mlist) (1+ mcount))
    )
)

;A function HOW-MANY-CHOICES that tells how many choices
;   Robbie has for where he may move given the current value of
;   the variable LOC.
(defun how-many-choices ()
    (setq mtemp loc)
    (count-for (choices mtemp) 0)
)

;The function LOOK that takes two inputs, a direction and a
;   room, and tells Robbie where he will end up if he moved in
;   that direction from that room.
(defun look (dir loc)
    (setq mtemp (choices loc))
    (if (listp mtemp)
        (searchfor mtemp dir)
    )
)
    