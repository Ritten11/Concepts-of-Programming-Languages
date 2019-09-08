class Rules() {

  //1. Remove number from possible value in row and column if its already set as solution
  //2. Remove all values from neighbors squares which are not possible
  //3. Remove all values from squares which are not possible

  def applyRules(matrix: SquareMatrix): SquareMatrix = {
    val matrixRow = removeRedundantValuesInRow(matrix)
    val matrixColumn = removeRedundantValuesInColumn(matrixRow)

    return matrixColumn
  }

  def updateNeighbours(square: Square, matrix: SquareMatrix, idx:Int):SquareMatrix = {
    if (idx == square.neighbours.length - 1){
      return matrix;
    }
    val neighbour = matrix.getSquare(square.neighbours(idx)(0),square.neighbours(idx)(1))
    if (square.isSolved) {
      val neighbourValues = List[Int](square.getCorrectValue()+1,square.getCorrectValue()-1)
      val newPossValues = neighbour.possibleValues.intersect(neighbourValues)
      updateNeighbours(square, matrix.setSquare(neighbour.setValues(newPossValues)),idx+1)
    }


    return updateNeighbours(square,matrix,idx+1);
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
