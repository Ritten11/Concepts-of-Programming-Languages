class SquareMatrix(s:Int,
                   square:List[Square]){

  val allSquares = square;
  val size:Int = s;

  def getAllFromX(i:Int):List[Square] = {
    return allSquares.filter(_.x == i)
  }

  def getAllFromY(i:Int):List[Square] = {
    return allSquares.filter(_.y == i)
  }

  def getSquare(x:Int,y:Int):Square = {
    return allSquares.filter(_.x == x).filter(_.y == y)(0)
  }

  def setValue(x:Int,y:Int,solution:Int, isStartValue:Boolean = false):SquareMatrix = {
    val s = getSquare(x,y)
    val newList = allSquares.filter(_ != s)
    return new SquareMatrix(this.size, newList :+ s.setValue(solution,isStartValue))
  }

 /* def updateNeighbours(x:Int, y:Int):Unit = { // das hier sollte nicht so ganz stimmen
    val currentSquare:Square = this.getSquare(x,y);
    for(s <- currentSquare.neighbours){
      val square = this.getSquare(s.x,s.y); // need to get it out of the list, otherwise different object
      allSquares = allSquares.filter((del:Square) => del.x!=square.x ||del.y!=square.y);
      val neighborList = square.neighbours.filter((n:Square) => (n.x!=currentSquare.x) || (n.y!=currentSquare.y)) :+ currentSquare;
      val tmpSquare = new Square(square.x, square.y,square.possibleValues,neighborList,(square.neighbours == 1));
      allSquares = allSquares:+tmpSquare;

    }
    println("bla")
  }

  */

  def setSquare(square:Square):SquareMatrix = {
    val s = getSquare(square.x,square.y);
    return new SquareMatrix(size, allSquares.filter(_ != s) :+ s);
  }

  def removeValue(x:Int,y:Int,wrongSolution:Int):SquareMatrix = {

    val s = getSquare(x,y)
    val newList = allSquares.filter(_ != s)
    return new SquareMatrix(this.size, newList:+ s.removeValue(wrongSolution))
  }

  def printIt(sizeP:Int): Unit = {
    for (y <- List.range(1,sizeP+1)) {
      for (x <- List.range(1,sizeP+1)) {
        val s = getSquare(x, y)
        printf("%15s", s.possibleValues.mkString(", ") + "(" + s.neighbours.length + ") |")
      }
      println("")
      println("---------------------------------------------------------------------------")
    }
  }
  def isCoordinateInRange(x:Int, y:Int):Boolean = {
    if(x>0 && y>0 && x<=size &&y<=size){
      return true;
    }
    return false;
  }

  def isValid(x:Int,y:Int,solution:Int):Boolean = {

    val checkedSquare:Square = this.getSquare(x,y);

    //neighbor checking
    for(array <- checkedSquare.neighbours){ //Check if neighbor is valid
      val xNeighbor = array(0);
      val yNeighbor = array(1);
     // val filterValues = s.possibleValues.filter((i:Int) => Math.abs(i-solution)==1);
      val mapValues = this.getSquare(xNeighbor,yNeighbor).possibleValues.map((i:Int) => Math.abs(i-solution));
      val filterValues = mapValues.filter(_==1);

      if(filterValues.isEmpty){
        return false
      }
    }

    //not neighbor checking
    val notNeighborsX =
      for(xDifference <- List(1,-1); if(isCoordinateInRange(x+xDifference,y)))
        yield Array(x+xDifference,y);

    val notNeighborsY =
      for(yDifference <- List(1,-1); if(isCoordinateInRange(x,y+yDifference)))
        yield Array(x,y+yDifference);

    val notNeighbors = (notNeighborsX ::: notNeighborsY).
      filter((a:Array[Int])=>(!listContainsArray(checkedSquare.neighbours,a)))

    for(coordinate <- notNeighbors){
      val filterValues = this.getSquare(coordinate(0),coordinate(1)).possibleValues.filter((i:Int) => Math.abs(i-solution)>1);
      if(filterValues.isEmpty){
        return false
      }
    }

    for(s<-(getAllFromX(x):::getAllFromY(y))){
      val oneSquare = s.asInstanceOf[Square];
      if(x!=oneSquare.x || y!=oneSquare.y){ //don't compare Square with itself
        if(oneSquare.isSolved==true && oneSquare.possibleValues(0)==solution){
          return false;
        }
      }
    }
    return true;
  }

  def removeIfNotValid(x:Int,y:Int,solution:Int):SquareMatrix = {
    if(!isValid(x,y,solution)){
      println("removing",x,y,solution)
      return removeValue(x,y,solution)
    }
    return this
  }

  /*for(x<-List(1,2,3,4)){
    for(y<-List(1,2,3,4)){
      for(s<-List(1,2,3,4)){
        removeIfNotValid(x,y,s);

      }
    }
  }

  // Pattern Matching

  def patternMatching(s1:Int, s2:Int, s3:Int, s4:Int):Int = {
    val l = List(s1,s2,s3).sortWith(_<_)
    return l match {
      case List(1,2,3) => 4;
      case List(1,2,4) => 3;
      case List(1,3,4) => 2;
      case List(2,3,4) => 1;
      case List(_,_,_) => s4
    }
  }

  def updateAllX(): Unit ={
    for(x<-List(1,2,3,4)){
      for(y<-List(1,2,3,4)){
        val l = List(1,2,3,4).filter(_!=y)
        var res = patternMatching(
          getSquare(x, l(0)).getCorrectValue(),
          getSquare(x, l(1)).getCorrectValue(),
          getSquare(x, l(2)).getCorrectValue(),
          getSquare(x,y).getCorrectValue()
        )
        if (res>0){
          setValue(x,y,res)
        }
      }
    }
  }

  def updatePossValuesRow(y:Int, setNumber:Int): Unit ={
    val row = getAllFromY(y).filter((s:Square) => !s.isSolved)
    //println(row)
    allSquares = allSquares.filter((s:Square) => !row.contains(s))
    //println(row.map((s:Square) => s.removeValue(setNumber)))
    allSquares  = allSquares ++ row.map((s:Square) => s.removeValue(setNumber))
  }

  def updatePossValuesColumn(x:Int, setNumber:Int): Unit ={
    val row = getAllFromX(x).filter((s:Square) => !s.isSolved)
    allSquares = allSquares.filter((s:Square) => !row.contains(s))
    allSquares  = allSquares ++ row.map((s:Square) => s.removeValue(setNumber))
  }*/

//  for (y <- 1 to 4) {
//    for (x <- 1 to 4) {
//      val s = getSquare(x, y)
//      if (s.isSolved)
//        updatePossValuesRow(y, s.getCorrectValue())
//      updatePossValuesColumn(x, s.getCorrectValue())
//    }
//  }


  def listContainsArray(list:List[Array[Int]], array:Array[Int]):Boolean = {
    for(l <-list){
      if(l(0) == array(0) && l(1) == array(1)){
        return true;
      }
    }
    return false;
  }
}
