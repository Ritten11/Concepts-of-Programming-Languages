import java.io.IOException
import java.io.FileInputStream
import java.io.FileNotFoundException


import scala.jdk.CollectionConverters._

class SquareMatrixReader() {
  val home: String = System.getProperty("user.home")

  def readMatrix():SquareMatrix =  {

    try {
      val protMat = Matrix.ProtoMatrix.parseFrom(new FileInputStream(home + "/Documents/RUG/UiA/CoPL/ProtoBuffers/Data/src/puzzle_unsolved.bin"));
      val size = protMat.getSize
      var squareList = List[Square]()
      for(protSquare <- protMat.getSquaresList.asScala) { squareList = squareList.appended(parseSquare(protSquare,size))}
      val squareMatrix = new SquareMatrix(size, squareList)
      return squareMatrix
    } catch {
      case ex: FileNotFoundException =>{
        println("Missing file exception")
      }

      case ex: IOException => {
        println("IO Exception")
      }
    }

    println("Oops, something went wrong...")
    return new SquareMatrix(1,List(new Square(1,1,List(1))))
  }

  def parseSquare(matrixSquare: Matrix.ProtoSquare, size: Int): Square = {
    val pos = matrixSquare.getPosition
    var solved = false
    val possVal =
      if(matrixSquare.getValue == 0) { List.range(1,size+1)}
      else {
        solved = true
        List(matrixSquare.getValue)
      }
    var neighbours = List[Array[Int]]()
    for (neighbour <- matrixSquare.getNeighboursList.asScala) { neighbours = neighbours :+ parseCoordinates(neighbour) }
    var nonNeighbours = List[Array[Int]]()
    for (nonNeighbour <- matrixSquare.getNotNeighboursList.asScala) { nonNeighbours = nonNeighbours :+ parseCoordinates(nonNeighbour) }
    return new Square(pos.getX,pos.getY,possVal, neighbours, nonNeighbours, solved)
  }

  def parseCoordinates(matrixCoord: Matrix.ProtoSquare.Coordinate):Array[Int] = {
    return Array(matrixCoord.getX,matrixCoord.getY)
  }

}
