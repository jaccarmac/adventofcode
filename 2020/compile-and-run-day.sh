DAY=$1

cd $DAY
$HOME/local/kotlin-compiler-1.4.20/bin/kotlinc $DAY.kt -include-runtime -d $DAY.jar
java -jar $DAY.jar
