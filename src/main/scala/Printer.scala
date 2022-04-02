case class Printer(x: Int, y: Int, angle: Int, board: Array[Array[String]]) {

  def printAndMove(value: String): Printer = {
    board(y)(x)=value
//    println(s"krok do przodu $x, $y")

    angle match {
      case 90 => Printer(x + 1, y, angle, board)
      case 180 => Printer(x, y + 1, angle, board)
      case -90 => Printer(x - 1, y, angle, board)
      case 0 => Printer(x, y - 1, angle, board)
    }
  }

  def rotate(): Printer = {
    if (angle + 90 == 270) Printer(x, y, -90,board)
    else Printer(x, y, angle + 90,board)
  }
}
