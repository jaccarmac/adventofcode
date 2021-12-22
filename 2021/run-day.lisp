(require 'asdf)
(require 'april)

(april:april-load (pathname "advent-of-april.apl"))

(defun print-solutions (day data)
  (with-open-file (input (format nil "data/~a" data))
    (let ((puzzle (make-string (file-length input))))
      (read-sequence puzzle input)
      (april:april (with (:store-val (p puzzle) (d day)))
                   "⍎'⎕←s',(⍕d),'a p◊⎕←s',(⍕d),'b p'"))))
