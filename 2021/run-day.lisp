(require 'asdf)
(require 'april)

(april:april-load (pathname "advent-of-april.apl"))

(defmacro print-solutions (day)
  `(with-open-file (input ,(format nil "data/~a" day))
     (let ((puzzle (make-string (file-length input))))
       (read-sequence puzzle input)
       (format t "~a~%~a~%"
               (april:april-c ,(format nil "s~aa" day) puzzle)
               (april:april-c ,(format nil "s~ab" day) puzzle)))))
