import java.io._;
object RunApp extends App {
  val inputdir = "Input"; // Change to real input dir
  val outputdir = "Output"; // Change to real output dir

  //var m = new SquareMatrix(0);

  val dir = new File(inputdir);
  println(dir)
  for(f<-dir.listFiles()){
    solveSlitherLinks(f)
  }

  def connectNeighbours(s1:Square,s2:Square, m:SquareMatrix):SquareMatrix = {
    m.allSquares = m.allSquares.filter((s:Square)=>s!=s1)
    m.allSquares = m.allSquares.filter((s:Square)=>s!=s2)
    m.allSquares = m.allSquares :+ s1.addNeighbour(s2.x,s2.y)
    m.allSquares = m.allSquares :+ s2.addNeighbour(s1.x,s1.y)
  }

  implicit def doubleToInt(i:Double):Int = {
    i.toInt
  }


  def isAllDigits(x: String) = x.map(Character.isDigit(_)).reduce(_&&_)

  def addLineToMatrix(index:Int, numberCount: Int,y:Int, line:List[String], m:SquareMatrix):SquareMatrix = {
    if(index >=line.length){
      return m; //TODO: is this correct?
    }
    val input:String = line(index);
    if(isAllDigits(input)){
      val m1 = m.setValue(numberCount+1,y,Integer.valueOf(input), true);
      return addLineToMatrix(index+1,numberCount+1,y,line, m1);
    }else if(input.equals("x")){
      val m1= connectNeighbours(m.getSquare(numberCount,y),m.getSquare(numberCount+1,y),m); //change to coordinates
      return addLineToMatrix(index+1, numberCount,y,line, m1);
    }else{
      return addLineToMatrix(index+1,numberCount+1,y,line, m);
    }
  }

  def initSquareList(size:Int):List[Square] = {
    var allSquares = List()[Square]
    for(xValue <- 1 to size){
      for(yValue <- 1 to size){
        val s = new Square(xValue,yValue,List.range(1,size+1));
        allSquares = allSquares :+ s;
      }
    }
    allSquares
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

  def initMatrix(numberSource:Array[List[String]], neighborSource:Array[List[String]], size:Int):SquareMatrix = { //TODO: implement multiple puzzles functionality
    val m = new SquareMatrix(size, initSquareList(size))
    for(line <- numberSource){ //TODO: Make it recursive
      addLineToMatrix(0,0,numberSource.indexOf(line)+1,line, m);
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

    val m = initMatrix(source._1, source._2, size);


    val a:BruteForce = new BruteForce(m);
    a.solve();
    m.printIt(m.size);

    val numPuzzles = lines(0)

    var out = new PrintWriter( new File(outputdir+"/"+f.getName()) , "UTF-8")
    out.print(numPuzzles + "\n")

    var sol = ""
    out.print("size " +  size + "x" + size + "\n");

    for (y <- List.range(1,size+1)) {
      for (x <- List.range(1,size+1)) {
        val s = m.getSquare(x, y)
        out.print(s.possibleValues(0) + " ");
      }
      out.print("\n")
    }
    out.close();
  }



}


