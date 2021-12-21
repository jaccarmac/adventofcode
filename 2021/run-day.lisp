(require 'asdf)
(require 'april)

(april:april-load (pathname "advent-of-april.apl"))

(defun print-solutions (day)
  (with-open-file (input (format nil "data/~a" day))
    (let ((puzzle (make-string (file-length input))))
      (read-sequence puzzle input)
      (april:april-c "solve" puzzle (write-to-string day)))))
