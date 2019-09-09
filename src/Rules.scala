class Rules() { //TODO: 1. Re-apply rules if square is set to solved (rule 1 is going to be tricky)
                //TODO: 2. Let BruteForce skip square if it is solved (the first TODO is a prerequisite for this)

  //1. Remove number from possible value in row and column if its already set as solution
  //2. Remove all values from neighbors squares which are not possible
  //3. Remove all values from squares which are not possible

  def applyRules(matrix: SquareMatrix): SquareMatrix = {
    val matrixRow = removeRedundantValuesInRows(matrix)
    val matrixColumn = removeRedundantValuesInColumns(matrixRow)

    return matrixColumn
  }

  def applyRules(matrix: SquareMatrix,
                 square: Square): SquareMatrix = {
    println("Original matrix: ")
    matrix.printIt()
    val matrixRow = removeRedundantValuesInRow(matrix, square)
    val matrixColumn = removeRedundantValuesInColumn(matrixRow, square)
    val matrixNeighbour = updateNeighbours(matrixColumn, square, 0)
    println("Updated matrix: ")
    matrixNeighbour.printIt()

    return matrixNeighbour
  }

  def updateNeighbours(matrix: SquareMatrix, square: Square, idx:Int):SquareMatrix = {
    if (idx > square.neighbours.length - 1){
      return matrix;
    }
    val neighbour = matrix.getSquare(square.neighbours(idx)(0),square.neighbours(idx)(1))
    if (neighbour.isSolved){
      return matrix;
    }
    println("Updating neighbour: " + neighbour.toString())
    if (square.isSolved) {
      val neighbourValues = List[Int](square.getCorrectValue()+1,square.getCorrectValue()-1)
      val newPossValues = neighbour.possibleValues.intersect(neighbourValues)
      println("newPossValues: " + newPossValues.mkString(" "))
      if (newPossValues.length == 1) {
        return updateNeighbours(applyRules(matrix.setSquare(neighbour.setValue(newPossValues(0))),neighbour),
          square,
          idx+1)
      } else {
        return updateNeighbours(matrix.setSquare(neighbour.setValues(newPossValues)), square, idx + 1)
      }
    }

    return updateNeighbours(matrix, square, idx+1);
  }

  def removeRedundantValuesInRow(matrix: SquareMatrix,
                                 square: Square): SquareMatrix = {
    val allSquares = matrix.allSquares.filter(_.x != square.x)
    val xSquares = matrix.getAllFromX(square.x)
    val newSquareList = getUpdatedSquares(xSquares) ::: allSquares
    return new SquareMatrix(matrix.size, newSquareList)
  }

  def removeRedundantValuesInColumn(matrix: SquareMatrix,
                                    square: Square): SquareMatrix = {
    val allSquares = matrix.allSquares.filter(_.y != square.y)
    val ySquares = matrix.getAllFromY(square.y)

    val newSquareList = getUpdatedSquares(ySquares) ::: allSquares
    return new SquareMatrix(matrix.size, newSquareList)
  }

  def removeRedundantValuesInRows(matrix: SquareMatrix): SquareMatrix = {
    val sizeP = matrix.allSquares.size
    var newSquareList = List[Square]()

    for (x <- 1 to sizeP) {
      val xSquares = matrix.getAllFromX(x)
      newSquareList = newSquareList ::: getUpdatedSquares(xSquares)
    }
    return new SquareMatrix(matrix.size, newSquareList)
  }

  def removeRedundantValuesInColumns(matrix: SquareMatrix): SquareMatrix = {
    val sizeP = matrix.allSquares.size
    var newSquareList = List[Square]()

    for (y <- 1 to sizeP) {
      val ySquares = matrix.getAllFromY(y)
      newSquareList = newSquareList ::: getUpdatedSquares(ySquares)
    }
    return new SquareMatrix(matrix.size, newSquareList)
  }

  private def getUpdatedSquares(squares: List[Square]): List[Square] = {
    val solvedSquares = squares.filter(_.isSolved)
    var updatedSquareList = List[Square]()

    var solutions = List[Int]()
    for (s <- solvedSquares) {
      solutions = solutions :+ s.possibleValues.head
    }

    updatedSquareList = updatedSquareList ::: solvedSquares
    val notSolved = squares.filter(!_.isSolved)
    for (s <- notSolved) {
      updatedSquareList = updatedSquareList :+ s.removeValues(solutions)
    }
    return updatedSquareList
  }
}
