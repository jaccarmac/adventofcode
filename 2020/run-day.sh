DAY=$1
PATH=$(pwd)/kotlinc/bin:$PATH

make kotlinc && touch kotlinc

cd $DAY
kotlinc $DAY.kt -include-runtime -d $DAY.jar
java -jar $DAY.jar
