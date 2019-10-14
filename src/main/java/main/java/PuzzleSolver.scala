import java.io._

object PuzzleSolver extends App {

  val inputdir = "puzzle_unsolved.txt"
  val outputdir = "puzzle_solved.txt"

 // val reader :SquareMatrixReader = new SquareMatrixReader()
 // reader.read

  val dir = new File(inputdir)
  solveSlitherLinks(dir)

  def connectNeighbours(s1: Square,
                        s2: Square,
                        m: SquareMatrix): SquareMatrix = {
    val newList = m.allSquares.filter((s: Square) => s != s1).filter((s: Square) => s != s2)
    return new SquareMatrix(m.size, newList :+ s1.addNeighbour(s2.x, s2.y) :+ s2.addNeighbour(s1.x, s1.y))
  }

  implicit def doubleToInt(i: Double): Int = {
    i.toInt
  }

  def isAllDigits(x: String) = x.map(Character.isDigit(_)).reduce(_ && _)

  def initSquareList(x: Int,
                     y: Int,
                     l: List[Square],
                     size: Int): List[Square] = {
    if (x > size) {
      return initSquareList(1, y + 1, l, size)
    }
    if (y > size) {
      return l
    }
    val notNeighboursX =
      for(xN <- List(1,-1) if isCoordinateInRange(x+xN,y,size))
        yield Array(x+xN, y)

    val notNeighboursY =
      for(yN <- List(1,-1) if isCoordinateInRange(x,y+yN,size))
        yield Array(x, y+yN)

    val notNeighbours = notNeighboursX ::: notNeighboursY;


    val s = new Square(x, y, List.range(1, size + 1), List(), notNeighbours)
    val tmpList = l :+ s
    return initSquareList(x + 1, y, tmpList, size)
  }

  def getInputAsLists(lines: Array[String]): (Array[List[String]], Array[List[String]]) = {
    val source = (lines.splitAt(1))._2
    val numberSourceTmp =
      for (i <- 0 to source.length - 1 if i % 2 == 0)
        yield source(i)
    val neighborSourceTmp =
      for (i <- 0 to source.length - 1 if i % 2 == 1)
        yield source(i)

    val numberSource =
      (for (entry <- numberSourceTmp)
        yield ((for (s <- entry.split(" ")) yield s).filter(!_.equals(""))).toList).toArray

    val neighborSource = neighborSourceTmp.toArray.
      map((s: String) => ((s.toArray).indices.collect { case i if i % 4 == 0 => s(i) })) //also filters line breaker

    val tmp = Tuple2(numberSource, neighborSource.map(_.toList.map(_.toString)))
    return tmp
  }

  def iterateOverNumberSource(i: Int,
                              numberSource: Array[List[String]],
                              m: SquareMatrix): SquareMatrix = {
    if (i == numberSource.length - 1) {
      return addLineToMatrix(0, 0, i + 1, numberSource(i), m)
    }
    val tmpMatrix = addLineToMatrix(0, 0, i + 1, numberSource(i), m)
    return iterateOverNumberSource(i + 1, numberSource, tmpMatrix)
  }

  def iterateOverNeighbourSource(i: Int,
                                 neighbourSource: Array[List[String]],
                                 m: SquareMatrix): SquareMatrix = {
    if (i == neighbourSource.length - 1) {
      return addNeighbourLineToMatrix(0, i + 1, neighbourSource(i), m)
    }
    val tmpMatrix = addNeighbourLineToMatrix(0, i + 1, neighbourSource(i), m)
    return iterateOverNeighbourSource(i + 1, neighbourSource, tmpMatrix)
  }

  def addLineToMatrix(index: Int,
                      numberCount: Int,
                      y: Int,
                      line: List[String],
                      m: SquareMatrix): SquareMatrix = {
    if (index >= line.length) {
      return m
    }
    val input: String = line(index)
    if (isAllDigits(input)) {
      val m1 = m.setValue(numberCount + 1, y, Integer.valueOf(input), true)
      return addLineToMatrix(index + 1, numberCount + 1, y, line, m1)
    } else if (input.equals("x")) {
      val m1 = connectNeighbours(m.getSquare(numberCount, y), m.getSquare(numberCount + 1, y), m) //change to coordinates
      return addLineToMatrix(index + 1, numberCount, y, line, m1)
    } else {
      return addLineToMatrix(index + 1, numberCount + 1, y, line, m)
    }
  }

  def addNeighbourLineToMatrix(index: Int,
                               y: Int,
                               line: List[String],
                               m: SquareMatrix): SquareMatrix = {
    if (index >= line.length) {
      return m
    }
    val input: String = line(index)
    if (input.equals("x")) {
      val s1: Square = m.getSquare(index + 1, y)
      val s2: Square = m.getSquare(index + 1, y + 1)
      val tmpMatrix = connectNeighbours(s1, s2, m)
      return addNeighbourLineToMatrix(index + 1, y, line, tmpMatrix)
    }
    return addNeighbourLineToMatrix(index + 1, y, line, m)
  }

  def initMatrix(numberSource: Array[List[String]],
                 neighborSource: Array[List[String]],
                 size: Int): SquareMatrix = {
    val m = new SquareMatrix(size, initSquareList(1, 1, List[Square](), size))
    val m2 = iterateOverNumberSource(0, numberSource, m)
    val m3 = iterateOverNeighbourSource(0, neighborSource, m2)
    return m3
  }

  def solveSlitherLinks(f: File): Unit = {
    println(f.getName())

    val puzzles = scala.io.Source.fromFile(f).mkString.split("size ")

    val numPuzzles = puzzles(0)

    val file = new File(outputdir)
    val out = new PrintWriter(file)

    out.print(numPuzzles)
    val t1 = System.nanoTime

    for (puzzle <- puzzles if puzzles.indexOf(puzzle) > 0) {
      val lines = puzzle.split("\n")
      val listSize = lines(0).split(" ").map((s: String) => s.toList.filter(_ >= ' ').mkString)
      val size = Integer.valueOf(listSize(0).split("x")(0))

      val source = getInputAsLists(lines)
      val m = initMatrix(source._1, source._2, size)

      val a: BruteForce = new BruteForce(m)
      val solved = a.solve()
      val matrix = solved._2


      out.print("size " + size + "x" + size + "\n")

      if ((matrix.allSquares.filter((s: Square) => s.isSolved == true)).length == (size * size)) {

        createBinFile(matrix, size) //transfer it to python

        for (y <- List.range(1, size + 1)) {
          for (x <- List.range(1, size + 1)) {
            val s = matrix.getSquare(x, y)
            out.print(s.possibleValues(0) + " ")
          }
          out.print("\n")
        }
      } else {
        out.print("This puzzle is not solvable\n")
      }

    }
    out.close()
  }


  def isCoordinateInRange(x: Int,
                          y: Int,
                          size: Int): Boolean = {
    if (x > 0 && y > 0 && x <= size && y <= size) {
      return true
    }
    return false
  }

  def createBinFile(matrix: SquareMatrix, size:Int) = {
    val protoWriter = new ProtoWriter(matrix, size)
    protoWriter.writeBin()
  }

}


