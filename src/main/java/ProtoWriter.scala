import java.io.FileOutputStream

import PuzzleSolver.createBinFile

class ProtoWriter(matrices:List[SquareMatrix]) {
  def writeBin(): Unit ={

    var solutions = SolvedPuzzle.Solutions.newBuilder()
    solutions = solutions.setNumberOfPuzzles(matrices.length)
    for (matrix <- matrices) {
      solutions = solutions.addPuzzles(buildPuzzle(matrix))
    }
    val solvedPuzzles = solutions.build()
    val fos = new FileOutputStream("puzzle_solved.bin");
    solvedPuzzles.writeTo(fos);

  }

  def buildPuzzle(matrix:SquareMatrix):SolvedPuzzle.Puzzle.Builder = {
    var puzzle = SolvedPuzzle.Puzzle.newBuilder()
    puzzle = puzzle.setSize(matrix.size)
    for (square <- matrix.allSquares) {
      puzzle.addSquares(buildSquare(square))
    }
    return puzzle
  }

  def buildSquare(scalaSquare: Square):SolvedPuzzle.Square.Builder = {
    var square = SolvedPuzzle.Square.newBuilder()
    square = square.setX(scalaSquare.x).setY(scalaSquare.y).setValue(scalaSquare.possibleValues(0))
    return square
  }

}
