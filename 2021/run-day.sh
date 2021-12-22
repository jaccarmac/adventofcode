DAY=$1
DATA=$2

sbcl --noinform --noprint --disable-debugger \
     --load run-day.lisp \
     --eval "(print-solutions $DAY \"$DATA\")" \
     --quit
