import scala.collection.mutable

object Spiral extends App {

  create(9)

  def create(n: Int): Array[Array[String]] = {
    val numbers = (1 to n * n).toList.map(_.toString)
    val numbersStrings = numbers.map { case n if (n.length <= numbers.last.length) => " " * (numbers.last.length - n.length) + n }
    val fields = Array.fill(n)(Array.fill(n)("x"))

    val moves = (for {
      i <- 1 until n
      k <- 1 to 2
    } yield i).reverse

    val allMoves = (n - 1) +: moves :+ 1

    val values = mutable.Stack[String]()
    values.pushAll(numbersStrings.reverse)
    val myPrinter = Printer(0, 0, 90, fields)

    val finalPrinter = allMoves.foldLeft(myPrinter) { (result, n) =>
      val nextPrinter =
        (0 until n).foldLeft(result) { (a, b) =>
          a.printAndMove(values.pop)
        }

      nextPrinter.rotate()
    }

    finalPrinter.board.map(row => row.mkString(" ")) foreach { row => row foreach print; println }
    finalPrinter.board
  }
}
