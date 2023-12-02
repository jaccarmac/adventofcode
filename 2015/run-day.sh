DAY=$1

declare -A solutions
solutions=( ["1"]="NotQuiteLisp")

cd $DAY
elm make ${solutions[$DAY]}.elm
