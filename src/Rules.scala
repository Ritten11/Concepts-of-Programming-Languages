class Rules() {
  //TODO: 1. Check if there are empty possValues to stop recursion
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
    val matrixRow = removeRedundantValuesInRow(matrix, square)
    val matrixColumn = removeRedundantValuesInColumn(matrixRow, square)
    val matrixNeighbour = updateNeighbours(matrixColumn, square, 0)

    return matrixNeighbour
  }

  def updateNeighbours(matrix: SquareMatrix,
                       square: Square,
                       idx: Int): SquareMatrix = {
    if (idx > square.neighbours.length - 1) {
      return matrix
    }
    val neighbour = matrix.getSquare(square.neighbours(idx)(0), square.neighbours(idx)(1))
    if (neighbour.isSolved) {
      return matrix
    }
    if (square.isSolved) {
      val neighbourValues = List[Int](square.getCorrectValue() + 1, square.getCorrectValue() - 1)
      val newPossValues = neighbour.possibleValues.intersect(neighbourValues)
      if (newPossValues.length == 1) {
        if (!matrix.isValid(neighbour.x, neighbour.y, newPossValues(0))){
          return matrix.setInvalidMatrix()
        }
        return updateNeighbours(applyRules(matrix.setSquare(neighbour.setValue(newPossValues(0))),neighbour),
          square,
          idx + 1)
      } else {
        return updateNeighbours(matrix.setSquare(neighbour.setValues(newPossValues)), square, idx + 1)
      }
    }

    return updateNeighbours(matrix, square, idx + 1)
  }

  def removeRedundantValuesInColumn(matrix: SquareMatrix,
                                    square: Square): SquareMatrix = {
    val allSquaresWithoutX = matrix.allSquares.filter(_.x != square.x)
    val xSquares = matrix.getAllFromX(square.x)
    val solvedX = xSquares.filter(_.isSolved)
    val updatedSquares = getUpdatedSquares(xSquares)
    val newSquareList = updatedSquares ::: allSquaresWithoutX ::: solvedX
    val newMatrix = new SquareMatrix(matrix.size, newSquareList)
    for (s <- updatedSquares.filter(_.isSolved) ) {
      if (!newMatrix.isValid(s.x, s.y, s.getCorrectValue())){
        return matrix.setInvalidMatrix()
      }
    }
    return reApplyRules(newMatrix, updatedSquares)
  }

  def removeRedundantValuesInRow(matrix: SquareMatrix,
                                 square: Square): SquareMatrix = {
    val allSquaresWithoutY = matrix.allSquares.filter(_.y != square.y)
    val ySquares = matrix.getAllFromY(square.y)
    val solvedY = ySquares.filter(_.isSolved)
    val updatedSquares = getUpdatedSquares(ySquares)
    val newSquareList = updatedSquares ::: allSquaresWithoutY ::: solvedY
    val newMatrix = new SquareMatrix(matrix.size, newSquareList)
    for (s <- updatedSquares.filter(_.isSolved) ) {
      if (!newMatrix.isValid(s.x, s.y, s.getCorrectValue())){
        return matrix.setInvalidMatrix()
      }
    }
    return reApplyRules(newMatrix, updatedSquares)
  }

  def removeRedundantValuesInRows(matrix: SquareMatrix): SquareMatrix = {
    return (new SquareMatrix(matrix.size, removeRedundantValuesInRows(0, List[Square](), matrix)))
  }

  def removeRedundantValuesInRows(index: Int,
                                  squareList: List[Square],
                                  matrix: SquareMatrix): List[Square] = {
    val sizeP = matrix.allSquares.size
    if (index == sizeP) {
      return squareList
    }
    val ySquares = matrix.getAllFromY(index + 1)
    val solvedSquares = ySquares.filter(_.isSolved)
    val tmpSquareList = squareList ::: getUpdatedSquares(ySquares) ::: solvedSquares
    return (removeRedundantValuesInRows(index + 1, tmpSquareList, matrix))
  }


  def removeRedundantValuesInColumns(matrix: SquareMatrix): SquareMatrix = {
    return (new SquareMatrix(matrix.size, removeRedundantValuesInColumns(0, List[Square](), matrix)))
  }

  def removeRedundantValuesInColumns(index: Int,
                                     squareList: List[Square],
                                     matrix: SquareMatrix): List[Square] = {
    val sizeP = matrix.allSquares.size
    if (index == sizeP) {
      return squareList
    }
    val xSquares = matrix.getAllFromX(index + 1)
    val solvedSquares = xSquares.filter(_.isSolved)
    val tmpSquareList = squareList ::: getUpdatedSquares(xSquares) ::: solvedSquares
    return (removeRedundantValuesInColumns(index + 1, tmpSquareList, matrix))
  }

  private def getUpdatedSquares(squares: List[Square]): List[Square] = {
    val solvedSquares = squares.filter(_.isSolved)

    val solutions: List[Int] =
      for (s <- solvedSquares)
        yield s.possibleValues.head

    val notSolved = squares.filter(!_.isSolved)

    val updatedSquareList: List[Square] =
      for (s <- notSolved)
        yield s.removeValues(solutions)

    return updatedSquareList
  }

  private def reApplyRules(matrix: SquareMatrix,
                           updateSquares: List[Square]): SquareMatrix = {
    val solvedSquares = updateSquares.filter(_.isSolved)
    if (solvedSquares.isEmpty) {//} || !matrix.isValid(solvedSquares(0).x,solvedSquares(0).y,solvedSquares(0).getCorrectValue())) {
      return matrix
    }
    return reApplyRules(applyRules(matrix, solvedSquares(0)), solvedSquares.splitAt(1)._2)
  }
}
