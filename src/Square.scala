class Square(xNumber:Int,yNumber:Int, neighboursList:List[Square]=List(),
             values:List[Int]=List(1,2,3,4),solved:Boolean=false) {
  val x = xNumber;
  val y = yNumber;
  val neighbours = neighboursList;
  val possibleValues = values;
  val isSolved = solved;

  override def toString() = {
    "x:"+x+" y:"+y + " "+ possibleValues.mkString(",") + " solved:" + isSolved;
  }

  def setValue(solution:Int):Square = {
    return new Square(this.x,this.y,this.neighbours, List(solution),true);
  }

  def removeValue(wrongSolution:Int): Square  ={
    val newlist = possibleValues.filter(_ != wrongSolution);
    if(newlist.length==1){
      return new Square(this.x,this.y, this.neighbours, newlist,solved=true);
    }
    return new Square(this.x,this.y,this.neighbours, newlist);
  }

  def getCorrectValue():Int = {
    if(this.isSolved){
      return  this.possibleValues(0)
    } else {
      return 0;
    }
  }
}
