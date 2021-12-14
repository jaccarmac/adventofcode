(require 'asdf)
(require 'april)

(april:april-load (pathname "advent-of-april.apl"))

(defmacro print-solutions (day)
  `(let ((problem (vector 199 200 208 210 200 207 240 269 260 263)))
    (format t "~a~%~a~%"
            (april:april-c ,(format nil "s~aa" day) problem)
            (april:april-c ,(format nil "s~ab" day) problem))))
