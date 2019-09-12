class BruteForce(val squareMatrix: SquareMatrix) {
  def solve(): Tuple2[Boolean, SquareMatrix] = {
    val rules = new Rules
    val matrix = rules.applyRules(squareMatrix)
    return recursionSolver(1, 1, matrix);
  }

  def recursionSolver(x: Int, y: Int, sMatrix: SquareMatrix): Tuple2[Boolean, SquareMatrix] = {

    if (!sMatrix.isCoordinateInRange(x, y)) {
      return (true, sMatrix);
    }
    val s: Square = sMatrix.getSquare(x, y);

    val (i, j): Tuple2[Int, Int] = //next step
      if (x == sMatrix.size) {
        (-sMatrix.size + 1, 1)
      } else {
        (1, 0)
      }

    for (startValue <- sMatrix.getSquare(x, y).possibleValues) {
      val tuple = tryNumbers(x, y, startValue, sMatrix)
      if (!tuple._1) {
        return (false, sMatrix);
      }
      //      startValue+=1;
      val tmp = recursionSolver(x + i, y + j, tuple._2);
      if (tmp._1) {
        return tmp
      }
    }


    return (false, sMatrix);
  }

  //improvable: change all values with just possible values
  def tryNumbers(x: Int, y: Int, startValue: Int, sMatrix: SquareMatrix): Tuple2[Boolean, SquareMatrix] = {
    for (number <- sMatrix.getSquare(x, y).possibleValues.filter(_ >= startValue)) { //TODO: changing "startValue to sMatrix.size" to "sMatrix.getSquare(x,y).possibleValues.filter(_ >= startValue)" makes the matrix unsolvable PROBLEM: possValues is not correct
      if (sMatrix.isValid(x, y, number)) {
        val m = sMatrix.setValue(x, y, number, false);
        val rules = new Rules
        val m2 = rules.applyRules(m, m.getSquare(x,y))
        return (m2.valSolution, m2);
      }
    }
    return (false, sMatrix);
  }
}
