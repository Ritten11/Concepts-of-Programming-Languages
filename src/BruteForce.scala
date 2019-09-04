class BruteForce(val squareMatrix: SquareMatrix) {
  def solve():Unit = {
    recursionSolver(1,1,squareMatrix);
  }

  def recursionSolver(x:Int, y:Int, sMatrix: SquareMatrix): Boolean = {

    if(!sMatrix.isCoordinateinRange(x,y)){
      return true;
    }
    val s:Square = sMatrix.getSquare(x,y);

    var startValue:Int = 1;

    val (i,j):Tuple2[Int,Int] =  //next step
    if(x == sMatrix.size){
      (-sMatrix.size+1,1)
    }else{
      (1,0)
    }
    var check:Boolean = false;

    while(check == false){
      if(s.isStartValue){
        return recursionSolver(x+i,y+j, sMatrix);
      }
        if(!tryNumbers(x,y, startValue, sMatrix)){//Wenn alles ausprobiert wurde, gehe zurück und probiere da alles zu aus
          sMatrix.setSquare(new Square(x,y,List.range(1,sMatrix.size+1), s.neighbours));
          return false;
        }
        startValue+=1;
        check = recursionSolver(x+i,y+j, sMatrix);

        if(check){
          return true;
        }
    }
    return true;
  }

  //improvable: change all values with just possible values
  def tryNumbers(x:Int, y:Int, startValue:Int, sMatrix: SquareMatrix):Boolean = {
    println("Looking at cell: (" + x + "," + y + ")")
    println("PossValues greater or equal to startValue " + startValue + ": " + sMatrix.getSquare(x,y).possibleValues.filter(_ >= startValue).mkString(" "))
    println("Instead trying numbers: " + (startValue to sMatrix.size).mkString(" ") + " while possValues: " + sMatrix.getSquare(x,y).possibleValues.mkString(" "))
    sMatrix.printIt(sMatrix.size)
    for(number <- sMatrix.getSquare(x,y).possibleValues.filter(_ >= startValue)){ //TODO: changing "startValue to sMatrix.size" to "sMatrix.getSquare(x,y).possibleValues.filter(_ >= startValue)" makes the matrix unsolvable PROBLEM: possValues is not correct
      print("Checking number: " + number)
      if(sMatrix.isValid(x,y,number)){
        println(" -> Valid number!")
        sMatrix.setValue(x,y,number,false);
        return true;
      }
      println("")
    }
    return false;
  }
}
