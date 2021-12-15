(require 'cl-seq)
(require 'subr-x)

(defun segmentspec->segment (segmentspec)
  (let ((direction (substring segmentspec 0 1))
        (distance (substring segmentspec 1)))
    (cond ((equal "R" direction) (cons :right (string-to-number distance)))
          ((equal "L" direction) (cons :left (string-to-number distance))))))

(equal '(:right . 2) (segmentspec->segment "R2"))
(equal '(:left . 10) (segmentspec->segment "L10"))

(defun segmentspec->steps (segmentspec)
  (let ((segment (segmentspec->segment segmentspec)))
    (append (list (car segment)) (make-list (cdr segment) :forward))))

(equal '(:right :forward :forward) (segmentspec->steps "R2"))
(equal '(:left :forward
               :forward
               :forward
               :forward
               :forward
               :forward
               :forward
               :forward
               :forward
               :forward) (segmentspec->steps "L10"))

(defun pathspec->path (pathspec)
  (apply #'append (mapcar #'segmentspec->steps (split-string pathspec ", "))))

(equal '(:right :forward :forward
                :left :forward :forward :forward) (pathspec->path "R2, L3"))

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

(defun walk-step (position step)
  (let ((x (car position))
        (y (cadr position))
        (old-direction (caddr position)))
    (cond ((eq :left step) (list x y (new-direction old-direction :left)))
          ((eq :right step) (list x y (new-direction old-direction :right)))
          ((eq :forward step) (cond
                               ((eq :north old-direction) (list x
                                                                (+ y 1)
                                                                old-direction))
                               ((eq :east old-direction) (list (+ x 1)
                                                               y
                                                               old-direction))
                               ((eq :south old-direction) (list x
                                                                (- y 1)
                                                                old-direction))
                               ((eq :west old-direction)
                                (list (- x 1)
                                      y
                                      old-direction)))))))

(defun howfarawayis (pathspec)
  (let ((destination (cl-reduce #'walk-step
                             (pathspec->path pathspec)
                             :initial-value '(0 0 :north))))
    (+ (abs (car destination)) (abs (cadr destination)))))

(= 5 (howfarawayis "R2, L3"))
(= 2 (howfarawayis "R2, R2, R2"))
(= 12 (howfarawayis "R5, L5, R5, R3"))

(defun solve-problem-1 ()
  (howfarawayis
   (with-temp-buffer
     (insert-file-contents "problem.txt")
     (string-trim (buffer-string)))))

(print (solve-problem-1))

(defun walk-step-until-cross (positions step)
  (let ((position (car positions))
        (old-positions (cdr positions)))
    (if (member (butlast position) (butlast old-positions))
        positions
      (cons (walk-step position step)
            (append old-positions (list (butlast position)))))))

(defun howfarawayiscross (pathspec)
  (let ((destination (car (cl-reduce #'walk-step-until-cross
                                  (pathspec->path pathspec)
                                  :initial-value '((0 0 :north) . nil)))))
    (+ (abs (car destination)) (abs (cadr destination)))))

(defun solve-problem-2 ()
  (howfarawayiscross
   (with-temp-buffer
     (insert-file-contents "problem.txt")
     (string-trim (buffer-string)))))

(print (solve-problem-2))
