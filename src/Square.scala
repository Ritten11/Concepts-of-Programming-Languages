class Square(xNumber: Int,
             yNumber: Int,
             values: List[Int],
             neighboursList: List[Array[Int]] = List(),
             solved: Boolean = false,
             startValue: Boolean = false) {
  val x = xNumber;
  val y = yNumber;
  val neighbours = neighboursList;
  val possibleValues = values;
  val isSolved = solved;
  val isStartValue = startValue; //not sure if it is necessary


  override def toString() = {
    "x:" + x + " y:" + y + " " + possibleValues.mkString(",") + "#Neighbors: " + neighbours.length + " solved:" + isSolved;
  }

  def setValue(solution: Int, startValue: Boolean = false): Square = {
    return new Square(this.x, this.y, List(solution), this.neighbours, true, startValue);
  }

  def setValues(solution: List[Int], startValue: Boolean = false): Square = {
    return new Square(this.x, this.y, solution, this.neighbours, true, startValue);
  }

  def addNeighbour(x: Int, y: Int): Square = {
    return new Square(this.x, this.y, this.possibleValues, this.neighbours :+ Array(x, y), this.solved, this.isStartValue)
  }

  def removeValue(wrongSolution: Int): Square = {
    val newlist = possibleValues.filter(_ != wrongSolution);
    if (newlist.length == 1) {
      return new Square(this.x, this.y, newlist, this.neighbours, solved = true);
    }
    return new Square(this.x, this.y, newlist, this.neighbours);
  }

  def removeValues(wrongSolutions: List[Int]): Square = {
    val newList = possibleValues.filter(!wrongSolutions.contains(_));
    if (newList.length == 1) {
      return new Square(this.x, this.y, newList, this.neighbours, solved = true);
    }
    return new Square(this.x, this.y, newList, this.neighbours);
  }

  def getCorrectValue(): Int = {
    if (this.isSolved) {
      return this.possibleValues(0)
    } else {
      return 0;
    }
  }
}
