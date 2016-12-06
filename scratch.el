(defun segmentspec->segment (segmentspec)
  (let ((direction (substring segmentspec 0 1))
        (distance (substring segmentspec 1)))
    (cond ((equal "R" direction) (cons :right (string-to-number distance)))
          ((equal "L" direction) (cons :left (string-to-number distance))))))

(equal '(:right . 2) (segmentspec->segment "R2"))
(equal '(:left . 10) (segmentspec->segment "L10"))

(defun pathspec->path (pathspec)
  (mapcar #'segmentspec->segment (split-string pathspec ", ")))

(equal '((:right . 2) (:left . 3)) (pathspec->path "R2, L3"))
(equal '((:right . 2) (:right . 2) (:right . 2)) (pathspec->path "R2, R2, R2"))
(equal '((:right . 5) (:left . 5) (:right . 5) (:right . 3))
       (pathspec->path "R5, L5, R5, R3"))

(defun new-direction (old-direction turn-direction)
  (cond ((eq :left turn-direction)
         (cond ((eq :north old-direction) :west)
               ((eq :east old-direction) :north)
               ((eq :south old-direction) :east)
               ((eq :west old-direction) :south)))
        ((eq :right turn-direction)
         (cond ((eq :north old-direction) :east)
               ((eq :east old-direction) :south)
               ((eq :south old-direction) :west)
               ((eq :west old-direction) :north)))))

(eq :east (new-direction :south :left))
(eq :east (new-direction :north :right))

(defun walk-segment (position segment)
  (let ((new-direction (new-direction (caddr position) (car segment)))
        (distance (cdr segment))
        (x (car position))
        (y (cadr position)))
    (cond ((eq :north new-direction) (list x
                                           (+ y distance)
                                           new-direction))
          ((eq :east new-direction) (list (+ x distance)
                                          y
                                          new-direction))
          ((eq :south new-direction) (list x
                                           (- y distance)
                                           new-direction))
          ((eq :west new-direction) (list (- x distance)
                                          y
                                          new-direction)))))

(defun howfarawayis (pathspec)
  (let ((destination (reduce #'walk-segment
                             (pathspec->path pathspec)
                             :initial-value '(0 0 :north))))
    (+ (abs (car destination)) (abs (cadr destination)))))

(= 5 (howfarawayis "R2, L3"))
(= 2 (howfarawayis "R2, R2, R2"))
(= 12 (howfarawayis "R5, L5, R5, R3"))
