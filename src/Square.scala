class Square(xNumber: Int,
             yNumber: Int,
             values: List[Int],
             neighboursList: List[Square] = List(),
             solved: Boolean = false) {
  val x = xNumber;
  val y = yNumber;
  val neighbours = neighboursList;
  val possibleValues = values;
  val isSolved = solved;

  override def toString() = {
    "x:" + x + " y:" + y + " " + possibleValues.mkString(",") + "Neighbors: " + neighbours.mkString(",") +" solved:" + isSolved;
  }

  def setValue(solution: Int): Square = {
    return new Square(this.x, this.y, List(solution), this.neighbours, true);
  }

  def addNeighbour(s: Square): Square = {
    return new Square(this.x, this.y, this.possibleValues, this.neighbours :+ s, this.solved)
  }

  def removeValue(wrongSolution: Int): Square = {
    val newlist = possibleValues.filter(_ != wrongSolution);
    if (newlist.length == 1) {
      return new Square(this.x, this.y, newlist, this.neighbours, solved = true);
    }
    return new Square(this.x, this.y, newlist, this.neighbours);
  }

  def getCorrectValue(): Int = {
    if (this.isSolved) {
      return this.possibleValues(0)
    } else {
      return 0;
    }
  }
}
