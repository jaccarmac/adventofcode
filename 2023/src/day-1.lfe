(defmodule day-1
  (export (one-one 1)
          (one-two 1))
  (import
   (from lists (sum 1))
   (rename lists ((map 2) mapcar))))

(defun first-number
  ((#B()) 0)
  (((binary h (t binary))) (when (and (>= h #\0) (>= #\9 h)))
   (- h #\0))
  (((binary "one" (t binary))) 1)
  (((binary "two" (t binary))) 2)
  (((binary "three" (t binary))) 3)
  (((binary "four" (t binary))) 4)
  (((binary "five" (t binary))) 5)
  (((binary "six" (t binary))) 6)
  (((binary "seven" (t binary))) 7)
  (((binary "eight" (t binary))) 8)
  (((binary "nine" (t binary))) 9)
  (((binary _ (t binary)))
   (first-number t)))

(defun first-numeral
  ((#B()) 0)
  (((binary h (t binary))) (when (and (>= h #\0) (>= #\9 h)))
   (- h #\0))
  (((binary _ (t binary)))
   (first-numeral t)))

(defun last-number (s) (last-number s 0))

(defun last-number
  ((#B() 0) 0)
  ((#B() acc)
   (- acc #\0))
  (((binary h (t binary)) acc) (when (and (>= h #\0) (>= #\9 h)))
   (last-number t h))
  (((binary "one" (t binary)) _) (last-number (binary #\e (t binary)) #\1))
  (((binary "two" (t binary)) _)  (last-number (binary #\o (t binary)) #\2))
  (((binary "three" (t binary)) _) (last-number (binary #\e (t binary)) #\3))
  (((binary "four" (t binary)) _) (last-number t #\4))
  (((binary "five" (t binary)) _) (last-number (binary #\e (t binary)) #\5))
  (((binary "six" (t binary)) _) (last-number t #\6))
  (((binary "seven" (t binary)) _) (last-number (binary #\n (t binary)) #\7))
  (((binary "eight" (t binary)) _) (last-number (binary #\t (t binary)) #\8))
  (((binary "nine" (t binary)) _) (last-number (binary #\e (t binary)) #\9))
  (((binary _ (t binary)) acc)
   (last-number t acc)))

(defun last-numeral (s) (last-numeral s 0))

(defun last-numeral
  ((#B() 0) 0)
  ((#B() acc)
   (- acc #\0))
  (((binary h (t binary)) acc) (when (and (>= h #\0) (>= #\9 h)))
   (last-numeral t h))
  (((binary _ (t binary)) acc)
   (last-numeral t acc)))

(defun one-one (lines)
  (let* ((s (self))
         (pids (mapcar (lambda (line)
                         (spawn_link (lambda ()
                                       (! s (tuple (self) (+ (* 10 (first-numeral line)) (last-numeral line)))))))
                       lines))
         )
    (sum (gather pids))))

(defun one-two (lines)
  (let* ((s (self))
         (pids (mapcar (lambda (line)
                         (spawn_link (lambda ()
                                       (! s (tuple (self) (+ (* 10 (first-number line)) (last-number line)))))))
                       lines)))
    (sum (gather pids))))

(defun gather
  ((()) ())
  ((`(,h . ,t))
   (receive ((tuple sender n) (when (== sender h))
             `(,n . ,(gather t))))))
