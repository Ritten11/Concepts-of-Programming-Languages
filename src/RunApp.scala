import java.io._

object RunApp extends App {
  val inputdir = "Input"; // Change to real input dir
  val outputdir = "Output"; // Change to real output dir


  val dir = new File(inputdir);
  //println(dir)
  for (f <- dir.listFiles()) {
    solveSlitherLinks(f)
  }

  def connectNeighbours(s1: Square, s2: Square, m: SquareMatrix): SquareMatrix = {
    val newList = m.allSquares.filter((s: Square) => s != s1).filter((s: Square) => s != s2)
    return new SquareMatrix(m.size, newList :+ s1.addNeighbour(s2.x, s2.y) :+ s2.addNeighbour(s1.x, s1.y))
  }

  implicit def doubleToInt(i: Double): Int = {
    i.toInt
  }


  def isAllDigits(x: String) = x.map(Character.isDigit(_)).reduce(_ && _)

  def initSquareList(x: Int, y: Int, l: List[Square], size: Int): List[Square] = {
    if (x > size) {
      return initSquareList(0, y + 1, l, size);
    }
    if (y > size) {
      return l;
    }

    val s = new Square(x, y, List.range(1, size + 1));
    val tmpList = l :+ s;
    return initSquareList(x + 1, y, tmpList, size);

  }

  def getInputAsLists(lines: Array[String]): Tuple2[Array[List[String]], Array[List[String]]] = {
    val source = (lines.splitAt(2))._2;
    val numberSourceTmp =
      for (i <- 0 to source.length - 1 if i % 2 == 0)
        yield source(i);
    val neighborSourceTmp =
      for (i <- 0 to source.length - 1 if i % 2 == 1)
        yield source(i);

    val numberSource =
      (for (entry <- numberSourceTmp)
        yield ((for (s <- entry.split(" ")) yield s).filter(!_.equals(""))).toList).toArray

    val neighborSource = neighborSourceTmp.toArray.
      map((s: String) => ((s.toArray).indices.collect { case i if i % 4 == 0 => s(i) })); //also filters line breaker

    val tmp = Tuple2(numberSource, neighborSource.map(_.toList.map(_.toString)));
    return tmp;
  }

  def iterateOverNumberSource(i: Int, numberSource: Array[List[String]], m: SquareMatrix): SquareMatrix = {
    if (i == numberSource.length - 1) {
      return addLineToMatrix(0, 0, i + 1, numberSource(i), m);
    }
    val tmpMatrix = addLineToMatrix(0, 0, i + 1, numberSource(i), m);
    return iterateOverNumberSource(i + 1, numberSource, tmpMatrix);
  }

  def iterateOverNeighbourSource(i: Int, neighbourSource: Array[List[String]], m: SquareMatrix): SquareMatrix = {
    if (i == neighbourSource.length - 1) {
      return addNeighbourLineToMatrix(0, i + 1, neighbourSource(i), m);
    }
    val tmpMatrix = addNeighbourLineToMatrix(0, i + 1, neighbourSource(i), m);
    return iterateOverNeighbourSource(i + 1, neighbourSource, tmpMatrix);
  }

  def addLineToMatrix(index: Int, numberCount: Int, y: Int, line: List[String], m: SquareMatrix): SquareMatrix = {
    if (index >= line.length) {
      return m;
    }
    val input: String = line(index);
    if (isAllDigits(input)) {
      val m1 = m.setValue(numberCount + 1, y, Integer.valueOf(input), true);
      return addLineToMatrix(index + 1, numberCount + 1, y, line, m1);
    } else if (input.equals("x")) {
      val m1 = connectNeighbours(m.getSquare(numberCount, y), m.getSquare(numberCount + 1, y), m); //change to coordinates
      return addLineToMatrix(index + 1, numberCount, y, line, m1);
    } else {
      return addLineToMatrix(index + 1, numberCount + 1, y, line, m);
    }
  }

  def addNeighbourLineToMatrix(index: Int, y: Int, line: List[String], m: SquareMatrix): SquareMatrix = {
    if (index >= line.length) {
      return m;
    }
    val input: String = line(index);
    if (input.equals("x")) {
      val s1: Square = m.getSquare(index + 1, y);
      val s2: Square = m.getSquare(index + 1, y + 1);
      val tmpMatrix = connectNeighbours(s1, s2, m);
      return addNeighbourLineToMatrix(index + 1, y, line, tmpMatrix);
    }
    return addNeighbourLineToMatrix(index + 1, y, line, m);
  }

  def initMatrix(numberSource: Array[List[String]], neighborSource: Array[List[String]], size: Int): SquareMatrix = { //TODO: implement multiple puzzles functionality
    val m = new SquareMatrix(size, initSquareList(1, 1, List[Square](), size))
    val m2 = iterateOverNumberSource(0, numberSource, m);
    val m3 = iterateOverNeighbourSource(0, neighborSource, m2);
    return m3
  }


  def solveSlitherLinks(f: File): Unit = {

    println(f.getName())
    val lines = scala.io.Source.fromFile(f).mkString.split("\n")


    val listSize = lines(1).split(" ").map((s: String) => s.toList.filter(_ >= ' ').mkString);
    val size = Integer.valueOf(listSize(1).split("x")(0));

    val source = getInputAsLists(lines);
    val m = initMatrix(source._1, source._2, size);
    m.printIt();
    println("")

    val a: BruteForce = new BruteForce(m);

    val startTime = System.nanoTime

    val solved = a.solve();

    //check if solved puzzle is right
    var test = true;
    for(a <- solved._2.allSquares){
      if(!solved._2.isValid(a.x,a.y,a.possibleValues(0))){
        test = false;
        println(a.x + " , " + a.y)
      }
    }
    println(test)

    val endTime = System.nanoTime

    val duration = endTime - startTime

    println("Time for solving: " + duration / 1000000000 + " seconds")

    solved._2.printIt();
    println("")
    println("")

    val numPuzzles = lines(0)

    val outDir = new File(outputdir)
    outDir.mkdir()
    val out = new PrintWriter(outputdir + "/" + f.getName(), "UTF-8")

    out.print(numPuzzles + "\n")

    out.print("size " + size + "x" + size + "\n");

    if((solved._2.allSquares.filter((s:Square)=>s.isSolved==true)).length==(size*size)){
      for (y <- List.range(1, size + 1)) {
        for (x <- List.range(1, size + 1)) {
          val s = solved._2.getSquare(x, y)
          out.print(s.possibleValues(0) + " ");
        }
        out.print("\n")
      }
    }else{
      out.print("This puzzle is not solvable");
    }
    out.close();


  }


}


