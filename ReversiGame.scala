import java.io._

trait Player{
  def onTurn(board: Board): Unit
}

class UndoException extends Exception
class ExitException extends Exception
class GameOverException extends Exception

class HumanPlayer extends Player{
  def onTurn(board: Board): Unit = {
    if(board.getMovablePos().isEmpty()){
      // パス
      println("あなたはパスです。")
      board.pass()
      return ()
    }

    val br = new BufferedReader(new InputStreamReader(System.in))

    // todo: continue 書き換え
    while(true){
      print("手を\"f5\"のように入力、もしくは(U:取消/X:終了)を入力してください:")
      val in = br.readLine()

      if(in.equalsIgnoreCase("U"))
        throw new UndoException
      
      if(in.equalsIgnoreCase("X"))
        throw new ExitException
      
      var p = new Point()
      try{
	p = new Point(in);
      }
      catch
      {
        case e => IllegalArgumentException
	  println("正しい形式で入力してください！")
	  continue
      }

      if(!board.move(p))
      {
	println("そこには置けません！")
	continue
      }
      
      if(board.isGameOver())
        throw new GameOverException();
      
      break
    }
  }
}

class AIPlayer extends Player{
  private var Ai = null

  def AIPlayer(){
    Ai = new AlphaBetaAI();
  }

  def onTurn(board: Board): Unit = {
    print("コンピュータが思考中...")
    Ai.move(board)
    println("完了")
    if(board.isGameOver())
      throw new GameOverException()
  }
}

class ReversiGame{
  def main(args: Array[String]){
    val player = new Array[Player](2)
    val current_player = 0
    val board = new ConsoleBoard()
    val reverse = false
    
    if(args.length > 0){
      // コマンドラインオプション -r が与えられるとコンピュータ先手にする
      if(args(0) == "-r")
        reverse = true
    }

    // 先手・後手の設定
    if(reverse){
      player(0) = new AIPlayer()
      player(1) = new HumanPlayer()
    }
    else{
      player(0) = new HumanPlayer()
      player(1) = new AIPlayer()
    }

    while(true){
      board.print()

      try{
	player(current_player).onTurn(board)
      }
      catch{
        case e: UndoException =>
          board.undo(); board.undo()
	  while(board.getMovablePos().isEmpty()){
            board.undo(); board.undo()
          }

	  continue
        case e: ExitException =>
	  return ()
        case e: GameOverException =>
          println("ゲーム終了")
	  print("黒石" + board.countDisc(Disc.BLACK) + " ")
	  println("白石" + board.countDisc(Disc.WHITE))

	  return ()
        case e: Exception =>
          // 予期しない例外
	  println("Unexpected exception: " + e)
	  return ()
      }

      // プレイヤーの交代
      current_player = (current_player + 1) % 2;
    }
  }
}
