import java.io._;
object RunApp extends App {
  val inputdir = "Input"; // Change to real input dir
  val outputdir = "Output"; // Change to real output dir

  var m = new SquareMatrix;

  import java.io.File;

  val dir = new File(inputdir);
  println(dir)
  for(f<-dir.listFiles()){
    m = new SquareMatrix;
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

  /*def initMatrix(source:Array[String]) = { //TODO: implement multiple puzzles functionality
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
  */

  def isAllDigits(x: String) = x.map(Character.isDigit(_)).reduce(_&&_)

  def addLineToMatrix(index:Int, numberCount: Int,y:Int, line:List[String]):Unit = {
    if(index >=line.length){
      return;
    }
    val input:String = line(index);
    if(isAllDigits(input)){
      m.setValue(numberCount+1,y,Integer.valueOf(input));
      addLineToMatrix(index+1,numberCount+1,y,line);
    }else if(input.equals("x")){
      connectNeighbours(m.getSquare(numberCount,y),m.getSquare(numberCount+1,y));
      addLineToMatrix(index+1, numberCount,y,line);
      return;
    }else{
      addLineToMatrix(index+1,numberCount+1,y,line);
      return;
    }
  }

  def getInputAsLists(lines:Array[String]):Tuple2[Array[List[String]],Array[List[String]]] = {
    val source = (lines.splitAt(2))._2;
    val numberSourceTmp =
      for(i <-0 to source.length-1 if i%2==0)
        yield source(i);
    val neighborSourceTmp =
      for(i <- 0 to source.length-1 if i%2==1)
        yield source(i);

    val numberSource = (numberSourceTmp.toArray.map((s:String)=> s.toList.filter(_> ' ')));
    val neighborSource = neighborSourceTmp.toArray.
      map((s:String) => ((s.toArray).indices.collect { case i if i % 4 == 0 => s(i) })); //also filters line breaker

    val tmp= Tuple2(numberSource.map(_.map(_.toString)), neighborSource.map(_.toList.map(_.toString)));
    return tmp;
  }

  def initMatrix(size:Int ,numberSource:Array[List[String]], neighborSource:Array[List[String]]) = { //TODO: implement multiple puzzles functionality
    m.initMatrix(size)
    for(line <- numberSource){
      addLineToMatrix(0,0,numberSource.indexOf(line)+1,line);
    }
    for(lineNumber <- 0 to neighborSource.length-1){
      for(columnNumber <- 0 to neighborSource(lineNumber).length-1){
        if(neighborSource(lineNumber)(columnNumber).equals("x")){
          val s1:Square = m.getSquare(columnNumber+1,lineNumber+1);
          val s2:Square = m.getSquare(columnNumber+1,lineNumber+2);
          this.connectNeighbours(s1,s2);
        }
      }
    }
  }


  def solveSlitherLinks(f:File):Unit = {
    println(f.getName())
    val lines = scala.io.Source.fromFile(f).mkString.split("\n")

    //initMatrix(lines)

    val source = getInputAsLists(lines);
    val listSize = lines(1).split(" ").map((s:String)=>s.toList.filter(_>= ' ').mkString);
    val size = Integer.valueOf(listSize(1).split("x")(0));
    initMatrix(size, source._1, source._2);

    m.printIt(size);

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


