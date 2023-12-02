(defmodule |2023|
  (export (main 1)))

;;; --------------------
;;; entry point function
;;; --------------------

(defun main
  (((list day data))
   (let (((tuple 'ok contents) (file:read_file (filename:join `("data" ,data)))))
     (lfe_io:format "~w~n" `(,(one-one (string:split contents "\n" 'all))))
     (erlang:halt 0))))

;;; -----------------------
;;; miscellaneous functions
;;; -----------------------

(defun first-number
  ((()) 0)
  (((cons h t)) (when (and (>= h #\0) (>= #\9 h)))
   (- h #\0))
  (((cons _ t))
   (first-number t)))

(defun last-number (s) (last-number s 0))

(defun last-number
  ((() 0) 0)
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
                          (lists:map #'binary_to_list/1 lines))))
    (lists:sum (gather pids))))

(defun gather
  ((()) ())
  (((cons h t))
   (receive ((tuple sender n) (when (== sender h))
             (cons n (gather t))))))
