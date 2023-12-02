DAY=$1

cd $DAY
emacs --quick --batch --eval '(load-file "scratch.el")'
