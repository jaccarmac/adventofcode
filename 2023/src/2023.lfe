(defmodule |2023|
  (export (main 1))
  (import
   (from day-1 (one-one 1))
   (from file (read_file 1))
   (from filename (join 1))
   (from lfe_io (format 2))
   (from string (split 3))))

(defun main
  (((list day data))
   (let (((tuple 'ok contents) (read_file (join `("data" ,data)))))
     (format "~w~n" `(,(one-one (split contents "\n" 'all))))
     (halt 0))))
