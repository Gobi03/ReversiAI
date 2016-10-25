import java.util.Scanner

object BoardTest {
  def main(args: Array[String]){
    val sc = new Scanner(System.in)

    val board = new ConsoleBoard
    board.init()

    while(true) {
      /* print the board */
      board.printing()
      /* numbers of stones */
      print("黒石:"   + board.countDisc(Disc.BLACK) + "　")
      print("白石:"   + board.countDisc(Disc.WHITE) + "　")
      print("空マス:" + board.countDisc(Disc.EMPTY) + "　")
      println()

      println("p: pass, u: undo")
      
      board.getCurrentColor() match{
        case Disc.BLACK => println("黒番")
        case Disc.WHITE => println("白番")
        case _ =>
            throw new Exception("WALL or EMPTY is here")
      }
      print("手を入力して下さい:")

      val in = sc.nextLine()

      var flag = true

      if(in == "p"){
        if(!board.pass){
          println("パスできません！")
        }
        flag = false
      }
      else if(in == "u"){
        board.undo()
        flag = false
      }

      var p = new Point()
      if(flag){
        try{
          p = new Point(in)
        }
        catch{
          case _: java.lang.IllegalArgumentException =>
            println("リバーシ形式の手を入力して下さい！")
            flag = false
        }
      }

      if(!board.move(p) && flag){
        println("そこには置けません！")
        flag = false
      }

      if(board.isGameOver() && flag){
        /* print the board */
        board.printing()
        /* numbers of stones */
        print("黒石:"   + board.countDisc(Disc.BLACK) + "　")
        print("白石:"   + board.countDisc(Disc.WHITE) + "　")
        print("空マス:" + board.countDisc(Disc.EMPTY) + "　")
        println()
        println()
        println("----------------ゲーム終了----------------")
        return ()
      }
    }
  }
}
