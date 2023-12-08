DAY=$1
PATH=$(pwd)/kotlinc/bin:$PATH

cd $DAY
kotlinc $DAY.kt -include-runtime -d $DAY.jar
java -jar $DAY.jar
