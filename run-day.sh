YEAR=$1
DAY=$2

cd $YEAR
guix shell -m manifest.scm -- ./run-day.sh $DAY
