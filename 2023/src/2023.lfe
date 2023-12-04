(defmodule |2023|
  (export (main 1))
  (import
   (from day-1
         (one-one 1)
         (one-two 1))
   (from day-2 (day-two 1))
   (from day-3 (three-one 1))
   (from file (read_file 1))
   (from filename (join 1))
   (from lfe_io (format 2))
   (from string (split 3))))

(defun main
  (((list day data))
   (let (((tuple 'ok contents) (read_file (join `("data" ,data)))))
     (case day
       ("1" (format "~w~n~w~n" `(,(one-one (split contents "\n" 'all)) ,(one-two (split contents "\n" 'all)))))
       ("2" (format "~w~n" `(,(day-two contents))))
       ("3" (format "~w~n" `(,(three-one contents)))))
     (halt 0))))
