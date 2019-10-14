import java.io.IOException
import java.io.FileInputStream
import java.io.FileNotFoundException

class SquareMatrixReader() {
  val home: String = System.getProperty("user.home")

  def read() {

    try {
      val protMat = Matrix.ProtoMatrix.parseFrom(new FileInputStream(home + "/Documents/RUG/UiA/CoPL/ProtoBuffers/Data/src/puzzle.bin"));
      val size = protMat.getSize
      val protSquareList = protMat.getSquaresList
      println("size: " + size + " squarelist value square 10 - x/y: " + protSquareList.get(10).getPosition.getX + protSquareList.get(10).getPosition.getY)
    } catch {
      case ex: FileNotFoundException =>{
        println("Missing file exception")
      }

      case ex: IOException => {
        println("IO Exception")
      }
    }

  }

  def parseSquare(matrixSquare: Matrix.ProtoSquare, size: Int): Square = {
    val pos = matrixSquare.getPosition
    val possVal =
      if(matrixSquare.getValue == -1) { List.range(1,size+1)}
      else {List(matrixSquare.getValue) }
    val neighbours = List()
    for (neighbour <- matrixSquare.getNeighboursList) { neighbours :+ parseCoordinates(neighbour) }
    val nonNeighbours =
    return new Square(pos.getX,pos.getY,possVal, neighbours)
  }

  def parseCoordinates(matrixCoord: Matrix.ProtoSquare.Coordinate):Array[Int] = {
    return Array(matrixCoord.getX,matrixCoord.getY)
  }

}
