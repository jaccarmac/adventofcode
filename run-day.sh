YEAR=$1
DAY=$2
DATA=${3:-$DAY}

cd $YEAR
guix shell --container --network --emulate-fhs --manifest=manifest.scm -- ./run-day.sh $DAY $DATA
