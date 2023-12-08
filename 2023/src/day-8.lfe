(defmodule day-8
  (export (day-eight 1)))

(defun day-eight (input)
  (let* ((lines (string:lexemes input "\n"))
         (`(,instructions . ,network) lines)
         (network (lc ((<- node network)) (string:lexemes node " =(,)")))
         (network (lists:foldl (match-lambda ((`(,node ,l ,r) network) (mset network node `#(,l ,r)))) #M() network)))
    `#(,(length (follow network #"AAA" (lambda (n) (== #"ZZZ" n)) instructions ()))
       ,(lists:foldl #'lcm/2 1
                     (lc ((<- node (lists:filter (match-lambda (((binary _ _ c)) (== #\A c))) (maps:keys network))))
                       (length (follow network node (match-lambda (((binary _ _ c)) (== #\Z c))) instructions ())))))))

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

(defun lcm (n m)
  (div (* n m) (gcd n m)))

(defun gcd
  ((0 0) 0)
  ((n n) n)
  ((n m) (when (> n m)) (gcd (- n m) m))
  ((n m) (when (< n m)) (gcd n (- m n))))
