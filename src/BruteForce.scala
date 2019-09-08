class BruteForce(val squareMatrix: SquareMatrix) {
  def solve():Tuple2[Boolean,SquareMatrix] = {
    return recursionSolver(1,1,squareMatrix);
  }

  def recursionSolver(x:Int, y:Int, sMatrix: SquareMatrix): Tuple2[Boolean,SquareMatrix] = {

    if(!sMatrix.isCoordinateInRange(x,y)){
      return (true,sMatrix);
    }
    val s:Square = sMatrix.getSquare(x,y);

    var startValue:Int = 1;

    val (i,j):Tuple2[Int,Int] =  //next step
    if(x == sMatrix.size){
      (-sMatrix.size+1,1)
    }else{
      (1,0)
    }

    if(s.isStartValue){
      return recursionSolver(x+i,y+j, sMatrix);
    }

    do{
      val tuple = tryNumbers(x,y,startValue,sMatrix)
      if(!tuple._1){
        return (false,sMatrix); //TODO: Can be removed if the recursion is fully functional
      }
      startValue+=1;
      val tmp = recursionSolver(x+i,y+j, tuple._2);
      if (tmp._1){
        return tmp
      }
    }while(true)

    
    return (true,sMatrix);
  }

  //improvable: change all values with just possible values
  def tryNumbers(x:Int, y:Int, startValue:Int, sMatrix: SquareMatrix):Tuple2[Boolean,SquareMatrix] = {
    println("Looking at cell: (" + x + "," + y + ")")
    println("PossValues greater or equal to startValue " + startValue + ": " + sMatrix.getSquare(x,y).possibleValues.filter(_ >= startValue).mkString(" "))
    println("Instead trying numbers: " + (startValue to sMatrix.size).mkString(" ") + " while possValues: " + sMatrix.getSquare(x,y).possibleValues.mkString(" "))
    for(number <- startValue to sMatrix.size){ //TODO: changing "startValue to sMatrix.size" to "sMatrix.getSquare(x,y).possibleValues.filter(_ >= startValue)" makes the matrix unsolvable PROBLEM: possValues is not correct
      print("Checking number: " + number)
      if(sMatrix.isValid(x,y,number)){
        println(" -> Valid number!")
        val m = sMatrix.setValue(x,y,number,false);
        m.printIt(m.size)
        return (true,m);
      }
      println("")
    }
    return (false,sMatrix);
  }
}
