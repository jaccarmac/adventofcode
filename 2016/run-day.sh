DAY=$1

cd $DAY
emacs -Q --batch --eval '(load-file "scratch.el")'
