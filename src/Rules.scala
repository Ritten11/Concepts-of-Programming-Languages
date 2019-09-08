class Rules() {

  //1. Remove number from possible value in row and column if its already set as solution
  //2. Remove all values from neighbors squares which are not possible
  //3. Remove all values from squares which are not possible

  def applyRules(matrix: SquareMatrix): SquareMatrix = {
    val matrixRow = removeRedundantValuesInRow(matrix)
    val matrixColumn = removeRedundantValuesInColumn(matrixRow)

    return matrixColumn
  }

  def removeRedundantValuesInRow(matrix: SquareMatrix): SquareMatrix = {
    val sizeP = matrix.allSquares.size
    var newSquareList = List[Square]()

    for (x <- 1 to sizeP) {
      val xSquares = matrix.getAllFromX(x)
      newSquareList = newSquareList ::: getUpdatedSquares(xSquares)
    }
    return new SquareMatrix(matrix.size, newSquareList)
  }

  def removeRedundantValuesInColumn(matrix: SquareMatrix): SquareMatrix = {
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
