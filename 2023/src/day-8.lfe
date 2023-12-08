(defmodule day-8
  (export (day-eight 1)))

(defun day-eight (input)
  (let* ((lines (string:lexemes input "\n"))
         (`(,instructions . ,network) lines)
         (network (lc ((<- node network)) (string:lexemes node " =(,)")))
         (network (lists:foldl (match-lambda ((`(,node ,l ,r) network) (mset network node `#(,l ,r)))) #M() network)))
    `#(,(length (follow network #"AAA" (lambda (n) (== #"ZZZ" n)) instructions ())))))

(defun next (network node instruction)
  (let ((`#(,l ,r) (mref network node)))
    (case instruction
      (#\L l)
      (#\R r))))

(defun follow (network start stop? instructions path)
  (if (funcall stop? start)
    path
    (let (((binary i (rest binary)) instructions))
      (follow network (next network start i) stop? (binary (rest binary) i) `(,start . ,path)))))
