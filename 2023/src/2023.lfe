(defmodule |2023|
  (export (main 1)))

;;; --------------------
;;; entry point function
;;; --------------------

(defun main (args)
  (lfe_io:format "~w~n" `(,(lists:sum (lists:map #'one-one/1 '["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]))))
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

(defun one-one (s) (+ (* 10 (first-number s)) (last-number s)))
