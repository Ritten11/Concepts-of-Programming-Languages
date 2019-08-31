class SquareMatrix {
  var allSquares = List[Square]();

  def initMatrix(size:Int) = {
    for(xvalue <- 1 to size){
      for(yvalue <- 1 to size){
        val s = new Square(xvalue,yvalue,List.range(1,size));
        allSquares = allSquares :+ s;
      }
    }
  }


//  val s:Square = 4;
//  implicit def intToSquare(i:Int):Square = {
//    new Square(1,1,1,List(i))
//  }

  def getAllFromX(i:Int):List[Square] = {
    return allSquares.filter(_.x == i)
  }

  def getAllFromY(i:Int):List[Square] = {
    return allSquares.filter(_.y == i)
  }

  def getSquare(x:Int,y:Int):Square = {
    return allSquares.filter(_.x == x).filter(_.y == y)(0)
  }

  def setValue(x:Int,y:Int,solution:Int) = {

    var s = getSquare(x,y)
    allSquares = allSquares.filter(_ != s)
    s = s.setValue(solution)
    allSquares = allSquares :+ s
  }

  def removeValue(x:Int,y:Int,wrongSolution:Int) = {

    var s = getSquare(x,y)
    allSquares = allSquares.filter(_ != s)
    s = s.removeValue(wrongSolution)
    allSquares = allSquares :+ s
  }

  def printIt(): Unit = {
    for (x <- List(1, 2, 3, 4)) {
      for (y <- List(1, 2, 3, 4)) {
        val s = getSquare(x, y)
        printf("%15s", s.possibleValues.mkString(", ") + " |")
      }
      println("")
      println("------------------------------------------------------------")
    }
  }

  def isValid(x:Int,y:Int,solution:Int):Boolean = { //TODO: Add neighbours rule
    for(s<-(getAllFromX(x):::getAllFromY(y))){
      var oneSquare = s.asInstanceOf[Square];
      if(x!=oneSquare.x || y!=oneSquare.y){
        if(oneSquare.isSolved==true && oneSquare.possibleValues(0)==solution){
          println("Has solution:"+oneSquare.x,oneSquare.y,oneSquare.possibleValues)
          return false;
        }
      }
    }
    return true;
  }

  def removeIfNotValid(x:Int,y:Int,solution:Int){
    if(!isValid(x,y,solution)){
      println("removing",x,y,solution)
      removeValue(x,y,solution)
    }
  }

  /*for(x<-List(1,2,3,4)){
    for(y<-List(1,2,3,4)){
      for(s<-List(1,2,3,4)){
        removeIfNotValid(x,y,s);

      }
    }
  }*/

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
  }

//  for (y <- 1 to 4) {
//    for (x <- 1 to 4) {
//      val s = getSquare(x, y)
//      if (s.isSolved)
//        updatePossValuesRow(y, s.getCorrectValue())
//      updatePossValuesColumn(x, s.getCorrectValue())
//    }
//  }
}
