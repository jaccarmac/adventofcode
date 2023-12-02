(defmodule |2023|
  (export (main 1))
  (import
   (from file (read_file 1))
   (from filename (join 1))
   (from lfe_io (format 2))
   (from lists (sum 1))
   (from string (split 3))))

;;; --------------------
;;; entry point function
;;; --------------------

(defun main
  (((list day data))
   (let (((tuple 'ok contents) (read_file (join `("data" ,data)))))
     (format "~w~n" `(,(one-one (split contents "\n" 'all))))
     (halt 0))))

;;; -----------------------
;;; miscellaneous functions
;;; -----------------------

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

(defun last-number (s) (last-number s 0))

(defun last-number
  ((#B() 0) 0)
  ((#B() acc)
   (- acc #\0))
  (((binary h (t binary)) acc) (when (and (>= h #\0) (>= #\9 h)))
   (last-number t h))
  (((binary "one" (t binary)) _) (last-number t #\1))
  (((binary "two" (t binary)) _) (last-number t #\2))
  (((binary "three" (t binary)) _) (last-number t #\3))
  (((binary "four" (t binary)) _) (last-number t #\4))
  (((binary "five" (t binary)) _) (last-number t #\5))
  (((binary "six" (t binary)) _) (last-number t #\6))
  (((binary "seven" (t binary)) _) (last-number t #\7))
  (((binary "eight" (t binary)) _) (last-number t #\8))
  (((binary "nine" (t binary)) _) (last-number t #\9))
  (((binary _ (t binary)) acc)
   (last-number t acc)))

(defun one-one (lines)
  (let* ((s (self))
         (pids (lists:map (lambda (line)
                            (spawn_link (lambda ()
                                          (! s (tuple (self) (+ (* 10 (first-number line)) (last-number line)))))))
                          lines)))
    (sum (gather pids))))

(defun gather
  ((()) ())
  (((cons h t))
   (receive ((tuple sender n) (when (== sender h))
             (cons n (gather t))))))
