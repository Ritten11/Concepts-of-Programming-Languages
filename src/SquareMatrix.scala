class SquareMatrix(s: Int,
                   square: List[Square]) {

  val allSquares = square;
  val size: Int = s;

  def getAllFromX(i: Int): List[Square] = {
    return allSquares.filter(_.x == i)
  }

  def getAllFromY(i: Int): List[Square] = {
    return allSquares.filter(_.y == i)
  }

  def getSquare(x: Int,
                y: Int): Square = {
    return allSquares.filter(_.x == x).filter(_.y == y)(0)
  }

  def setValue(x: Int,
               y: Int,
               solution: Int,
               isStartValue: Boolean = false): SquareMatrix = {
    val s = getSquare(x, y)
    val newList = allSquares.filter(_ != s)
    return new SquareMatrix(this.size, newList :+ s.setValue(solution, isStartValue))
  }

  def setSquare(square: Square): SquareMatrix = {
    val s = getSquare(square.x, square.y);
    return new SquareMatrix(size, allSquares.filter(_ != s) :+ square);
  }

  //TODO Remove since its handled in Square
  def removeValue(x: Int,
                  y: Int,
                  wrongSolution: Int): SquareMatrix = {
    val s = getSquare(x, y)
    val newList = allSquares.filter(_ != s)
    return new SquareMatrix(this.size, newList :+ s.removeValue(wrongSolution))
  }

  def printIt(): Unit = {
    for (y <- List.range(1, size + 1)) {
      for (x <- List.range(1, size + 1)) {
        val s = getSquare(x, y)
        printf("%15s", s.possibleValues.mkString(", ") + "(" + s.neighbours.length + ") |")
      }
      println("")
      println("---------------------------------------------------------------------------")
    }
  }

  def isCoordinateInRange(x: Int,
                          y: Int): Boolean = {
    if (x > 0 && y > 0 && x <= size && y <= size) {
      return true;
    }
    return false;
  }

  def isValid(x: Int,
              y: Int,
              solution: Int): Boolean = {

    val checkedSquare: Square = this.getSquare(x, y);

    for (s <- (getAllFromX(x) ::: getAllFromY(y))) {
      val oneSquare = s.asInstanceOf[Square];
      if (x != oneSquare.x || y != oneSquare.y) { //don't compare Square with itself
        if (oneSquare.isSolved == true && oneSquare.possibleValues(0) == solution) {
          return false;
        }
      }
    }

    //neighbor checking
    for (array <- checkedSquare.neighbours) { //Check if neighbor is valid
      val xNeighbor = array(0);
      val yNeighbor = array(1);
      // val filterValues = s.possibleValues.filter((i:Int) => Math.abs(i-solution)==1);
      val mapValues = this.getSquare(xNeighbor, yNeighbor).possibleValues.map((i: Int) => Math.abs(i - solution));
      val filterValues = mapValues.filter(_ == 1);

      if (filterValues.isEmpty) {
        return false
      }
    }

    //not neighbor checking
    val notNeighborsX =
      for (xDifference <- List(1, -1); if (isCoordinateInRange(x + xDifference, y)))
        yield Array(x + xDifference, y);

    val notNeighborsY =
      for (yDifference <- List(1, -1); if (isCoordinateInRange(x, y + yDifference)))
        yield Array(x, y + yDifference);

    val notNeighbors = (notNeighborsX ::: notNeighborsY).
      filter((a: Array[Int]) => (!listContainsArray(checkedSquare.neighbours, a)))

    for (coordinate <- notNeighbors) {
      val filterValues = this.getSquare(coordinate(0), coordinate(1)).possibleValues.filter((i: Int) => Math.abs(i - solution) > 1);
      if (filterValues.isEmpty) {
        return false
      }
    }
    return true;
  }

  //TODO Remove since this is now done in the Rules
  def removeIfNotValid(x: Int,
                       y: Int,
                       solution: Int): SquareMatrix = {
    if (!isValid(x, y, solution)) {
      println("removing", x, y, solution)
      return removeValue(x, y, solution)
    }
    return this
  }

  def listContainsArray(list: List[Array[Int]],
                        array: Array[Int]): Boolean = {
    for (l <- list) {
      if (l(0) == array(0) && l(1) == array(1)) {
        return true;
      }
    }
    return false;
  }
}
