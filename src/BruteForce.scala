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
    for(number <- startValue to sMatrix.size){
      if(sMatrix.isValid(x,y,number)){
        sMatrix.setValue(x,y,number,false);
        return true;
      }
    }
    return false;
  }
}
