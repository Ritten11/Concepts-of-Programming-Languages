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

  def parseSquare(matrix: Matrix.ProtoSquare)

}
