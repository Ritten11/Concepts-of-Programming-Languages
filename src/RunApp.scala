import java.io._;
object RunApp extends App {
  val inputdir = "Input"; // Change to real input dir
  val outputdir = "Output"; // Change to real output dir

  var m = new SquareMatrix;

  import java.io.File;

  val dir = new File(inputdir);
  println(dir)
  for(f<-dir.listFiles()){
    solveSlitherLinks(f)
  }

  def connectNeighbours(s1:Square,s2:Square) = {
    m.allSquares = m.allSquares.filter((s:Square)=>s!=s1)
    m.allSquares = m.allSquares.filter((s:Square)=>s!=s2)
    m.allSquares = m.allSquares :+ s1.addNeighbour(s2)
    m.allSquares = m.allSquares :+ s2.addNeighbour(s1)
  }

  implicit def doubleToInt(i:Double):Int = {
    i.toInt
  }

  def initMatrix(source:Array[String]) = { //TODO: implement multiple puzzles functionality
    val size = source(1).charAt(source(1).length-1)
    m.initMatrix(size.asDigit)
    for (lineNr <- 2 until (source.length) by 2) {
      println("Current lineNr: " + lineNr)
      val line = source(lineNr)
      for (i <- 0 until line.length by 4){
        val char = source(lineNr).charAt(i)
        val y = lineNr/2
        if ( !char.toString.equals("_")) {
          val x = i/4 +1
          println("x: " + x + " y: " + y)
          m.setValue(x,y,char.asDigit)
        }
        if (i+2<source(lineNr).length) {
          val char2 = source(lineNr).charAt(i + 2)
          if (char2.toString.equals("x")) {
            val x = i/4 +1
            //println("Connecting neighbours: (" + x +"," + y +")")
            connectNeighbours(m.getSquare(x,y),m.getSquare(x+1,y))
          }
        }
      }
    }
  }

  def solveSlitherLinks(f:File):Unit = {
    println(f.getName())
    val lines = scala.io.Source.fromFile(f).mkString.split("\n")
    initMatrix(lines)
    m.printIt()
    val numPuzzles = lines(0)

    var out = new PrintWriter( new File(outputdir+"/"+f.getName()) , "UTF-8")
    out.print(numPuzzles + "\n")

    var sol = ""
    out.print("puzzles 2\n");
    out.print("size 4x4\n");

    out.print("1 3 2 4\n");

    out.print("4 2 3 1\n");

    out.print("3 4 1 2\n");

    out.print("2 1 4 3\n");

    out.print("size 5x5\n");

    out.print("4 3 5 2 1\n");

    out.print("2 4 3 1 5\n");

    out.print("1 2 4 5 3\n");

    out.print("3 5 1 4 2\n");

    out.print("5 1 2 3 4\n");


    out.close();
  }



}


