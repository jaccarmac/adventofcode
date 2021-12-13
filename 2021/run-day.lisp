(require 'asdf)
(require 'april)

(april:april-load (pathname "advent-of-april.apl"))

(defun print-solutions (day)
  (let ((problem (vector 199 200 208 210 200 207 240 269 260 263)))
    (format t "~a~%~a~%"
            (april:april-c "s1a" problem)
            (april:april-c "s1b" problem))))
