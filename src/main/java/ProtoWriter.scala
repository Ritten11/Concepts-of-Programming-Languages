import java.io.FileOutputStream

import PuzzleSolver.createBinFile

class ProtoWriter(matrix:SquareMatrix, size:Int) {
  def writeBin(): Unit ={
    var out = "puzzles 1\nsize " + size + "x" + size + "\n"
    for (y <- List.range(1, size + 1)) {
      for (x <- List.range(1, size + 1)) {
        val s = matrix.getSquare(x, y)
        out += s.possibleValues(0) + " "
      }
      out += "\n"
    }

    val solvedPuzzle = SolvedPuzzle.Puzzle.newBuilder().setPuzzle(out).build()
    val fos = new FileOutputStream("puzzle_solved.bin");
    solvedPuzzle.writeTo(fos);

  }
}
