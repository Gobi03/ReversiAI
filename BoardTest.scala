import java.util.Scanner

class ConsoleBoard extends Board {
  def prinring() {
    println("　a b c d e f g h ")
    for(i <- 1 to 8){
      print(" " + y)
      for(x <- 1 to 8){
        getColor(new Point(x, y)) match {
          case Disc.BLACK => print("●")
          case Disc.WHITE => print("◯")
          case _          => print("　")
        }
      }
      println()
    }
  }
}


class BoardTest {
  def main(args: Array[String]){

    // input system

    val board = new ConsoleBoard

    while(true) {
      board.printing()
      print("黒石"   + board.countDisc(Disc.BLACK) + "　")
      print("白石"   + board.countDisc(Disc.WHITE) + "　")
      print("空マス" + board.countDisc(Disc.EMPTY) + "　")
      println()

      print("手を入力して下さい。")

      try {
        val in = sc.nextInt()
      } catch {
        case 変数:例外クラス => 式1
          …
      } finally {
        式
      }
    }
  }
}
