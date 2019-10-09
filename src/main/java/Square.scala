class Square(xNumber: Int,
             yNumber: Int,
             values: List[Int],
             neighboursList: List[Array[Int]] = List(),
             notNeighboursList: List[Array[Int]] = List(),
             solved: Boolean = false){
  val x = xNumber
  val y = yNumber
  val neighbours = neighboursList
  val possibleValues = values
  val isSolved = solved
  val notNeighbours = notNeighboursList



  override def toString() = {
    "x:" + x + " y:" + y + " " + possibleValues.mkString(",") + "#Neighbors: " + neighbours.length + " solved:" + isSolved
  }

  def setValue(solution: Int,
               startValue: Boolean = false): Square = {
    return new Square(this.x, this.y, List(solution), this.neighbours, this.notNeighbours,true)
  }

  def setValues(solution: List[Int],
                startValue: Boolean = false): Square = {
    return new Square(this.x, this.y, solution, this.neighbours,this.notNeighbours, false)
  }

  def addNeighbour(x: Int,
                   y: Int): Square = {
    val newNoNeighboursList = this.notNeighbours.filter((a:Array[Int]) => (a(0)!=x || a(1)!=y))
    val newNeighboursList = this.neighbours :+ Array(x, y)
    return new Square(this.x, this.y, this.possibleValues, newNeighboursList,newNoNeighboursList, this.solved)
  }

  def removeValue(wrongSolution: Int): Square = {
    val newlist = possibleValues.filter(_ != wrongSolution)
    if (newlist.length == 1) {
      return new Square(this.x, this.y, newlist, this.neighbours,this.notNeighbours, solved = true)
    }
    return new Square(this.x, this.y, newlist, this.neighbours, this.notNeighbours)
  }

  def removeValues(wrongSolutions: List[Int]): Square = {
    val newList = possibleValues.filter(!wrongSolutions.contains(_))
    if (newList.length == 1) {
      return new Square(this.x, this.y, newList, this.neighbours,this.notNeighbours, solved = true)
    }
    return new Square(this.x, this.y, newList,this.neighbours, this.notNeighbours)
  }

  def getCorrectValue(): Int = {
    if (this.isSolved) {
      return this.possibleValues(0)
    } else {
      return 0
    }
  }
}
