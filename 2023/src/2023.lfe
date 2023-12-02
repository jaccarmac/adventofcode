(defmodule |2023|
  (export (main 1)))

;;; --------------------
;;; entry point function
;;; --------------------

(defun main (args)
  (lfe_io:format "~w~n" `(,(one-one '["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"])))
  (erlang:halt 0))

;;; -----------------------
;;; miscellaneous functions
;;; -----------------------

(defun first-number
  (((cons h t)) (when (and (>= h #\0) (>= #\9 h)))
   (- h #\0))
  (((cons _ t))
   (first-number t)))

(defun last-number (s) (last-number s ()))

(defun last-number
  ((() acc)
   (- acc #\0))
  (((cons h t) acc) (when (and (>= h #\0) (>= #\9 h)))
   (last-number t h))
  (((cons _ t) acc)
   (last-number t acc)))

(defun one-one (lines)
  (let* ((s (self))
         (pids (lists:map (lambda (line)
                            (spawn_link (lambda ()
                                          (! s (tuple (self) (+ (* 10 (first-number line)) (last-number line)))))))
                          lines)))
    (lists:sum (gather pids))))

(defun gather
  ((()) ())
  (((cons h t))
   (receive ((tuple sender n) (when (== sender h))
             (cons n (gather t))))))
