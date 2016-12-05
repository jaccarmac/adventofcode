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

(defun howfarawayis (pathspec)
  5)

(= 5 (howfarawayis "R2, L3"))
(= 2 (howfarawayis "R2, R2, R2"))
(= 12 (howfarawayis "R5, L5, R5, R3"))
