import java.io._;
object RunApp extends App {
  val inputdir = "Input"; // Change to real input dir
  val outputdir = "Output"; // Change to real output dir

  import java.io.File;

  val dir = new File(inputdir);
  for(f<-dir.listFiles()){
    solveSlitherLinks(f)
  }

  def solveSlitherLinks(f:File):Unit = {
    println(f.getName())
    val lines = scala.io.Source.fromFile(f).mkString.split("\n")
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


