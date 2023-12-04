(defmodule day-1
  (export (one-one 1)
          (one-two 1))
  (import
   (from lists (sum 1))
   (rename lists ((map 2) mapcar))))

(defun first-number
  (((binary "one" (t binary)) _) 1)
  (((binary "two" (t binary)) _) 2)
  (((binary "three" (t binary)) _) 3)
  (((binary "four" (t binary)) _) 4)
  (((binary "five" (t binary)) _) 5)
  (((binary "six" (t binary)) _) 6)
  (((binary "seven" (t binary)) _) 7)
  (((binary "eight" (t binary)) _) 8)
  (((binary "nine" (t binary)) _) 9)
  ((b f) (first-numeral b f)))

(defun first-numeral
  ((#B() _) 0)
  (((binary h (t binary)) _) (when (and (>= h #\0) (>= #\9 h)))
   (- h #\0))
  (((binary _ (t binary)) f)
   (funcall f t f)))

(defun last-number (s) (last-number s 0 #'last-number/3))

(defun last-number
  (((binary "one" (t binary)) _ f) (last-number (binary #\e (t binary)) #\1 f))
  (((binary "two" (t binary)) _ f)  (last-number (binary #\o (t binary)) #\2 f))
  (((binary "three" (t binary)) _ f) (last-number (binary #\e (t binary)) #\3 f))
  (((binary "four" (t binary)) _ f) (last-number t #\4 f))
  (((binary "five" (t binary)) _ f) (last-number (binary #\e (t binary)) #\5 f))
  (((binary "six" (t binary)) _ f) (last-number t #\6 f))
  (((binary "seven" (t binary)) _ f) (last-number (binary #\n (t binary)) #\7 f))
  (((binary "eight" (t binary)) _ f) (last-number (binary #\t (t binary)) #\8 f))
  (((binary "nine" (t binary)) _ f) (last-number (binary #\e (t binary)) #\9 f))
  ((b acc f) (last-numeral b acc f)))

(defun last-numeral (s) (last-numeral s 0 #'last-numeral/3))

(defun last-numeral
  ((#B() 0 _) 0)
  ((#B() acc _)
   (- acc #\0))
  (((binary h (t binary)) acc f) (when (and (>= h #\0) (>= #\9 h)))
   (funcall f t h f))
  (((binary _ (t binary)) acc f)
   (funcall f t acc f)))

(defun one-one (lines)
  (let* ((s (self))
         (pids (mapcar (lambda (line)
                         (spawn_link (lambda ()
                                       (! s `#(,(self) ,(+ (* 10 (first-numeral line #'first-numeral/2)) (last-numeral line)))))))
                       lines))
         )
    (sum (gather pids))))

(defun one-two (lines)
  (let* ((s (self))
         (pids (mapcar (lambda (line)
                         (spawn_link (lambda ()
                                       (! s `#(,(self) ,(+ (* 10 (first-number line #'first-number/2)) (last-number line)))))))
                       lines)))
    (sum (gather pids))))

(defun gather
  ((()) ())
  ((`(,h . ,t))
   (receive (`#(,sender ,n) (when (== sender h))
             `(,n . ,(gather t))))))
