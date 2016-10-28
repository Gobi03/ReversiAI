class AlphaBetaAI extends AI {
  class Move
    (override val x: Int, override val y: Int, e: Int)
      extends Point {
    val eval = e
    def this() = this(0, 0, 0)
  }

  def move(board: Board): Unit = {
    // Vector[Disc]
    var movables: scala.collection.immutable.Vector[Point]
                           = board.getMovablePos()

    if(movables.isEmpty) {
      // 打てる箇所がなければパスする
      board.pass()
      return ()
    }
    else if(movables.length == 1) {
      // 打てる箇所が一箇所だけなら探索は行われず、即座に打つ
      board.move(movables(0))
      return ()
    }
    else {
      var limit = 0
      var Eval = new MidEvaluator()
      // 事前に手を良さそうな順にソート
      movables = sort(board, movables, presearch_depth)

      
      // 終盤読み
      if(Board.MAX_TURNS - board.getTurns() <= wld_depth) {
        limit = Int.MaxValue
        if(Board.MAX_TURNS - board.getTurns() <= perfect_depth)
          Eval = new PerfectEvaluator()
        else
          Eval = new WLDEvaluator()
      }
      else { // 序盤・中盤
        limit = normal_depth
      }

      var eval_max = -Int.MaxValue
      var p = new Point()

      for(i <- 0 until movables.length){
        board.move(movables(i))
        val eval = -alphabeta(board, limit,
                          -Int.MaxValue, Int.MaxValue)
        board.undo()

        if(eval > eval_max){
          eval_max = eval

        }
      }

      board.move(p)
    }
  }

  def alphabeta(board: Board, limit: Int,
                  alphaArg: Int, beta: Int): Int = {
    var alpha = alphaArg

    // 深さ制限に達したら評価値を返す
    if(board.isGameOver() || limit == 0)
      return evaluate(board)
    else{
      // Vector[Disc]
      val pos = board.getMovablePos()

      if(pos.isEmpty){  // pass
        board.pass()
        // パスの場合探索ターン進めない
        val eval = -alphabeta(board, limit, -beta, -alpha)
        board.undo()
        return eval
      }
      else{
        for(i <- 0 until pos.length){
          board.move(pos(i))
          val eval = -alphabeta(board, limit-1, -beta, -alpha)
          board.undo()

          alpha = Math.max(alpha, eval)

          if(alpha >= beta){
            // β刈り
            return alpha
          }
        }

        return alpha
      }
    }
  }

  private def sort(board: Board, movables: scala.collection.immutable.Vector[Point], limit: Int): scala.collection.immutable.Vector[Point] = {
    var moves = Vector.empty[Move]

    for(i <- 0 until movables.length){
      val p = movables(i)

      board.move(p)
      val eval = -alphabeta(board, limit,
                          -Int.MaxValue, Int.MaxValue)
      board.undo()

      val move = new Move(p.x, p.y, eval)
      moves = moves :+ move
    }

    // 評価値の大きい順にソート(データが小さいかつ簡単のため、選択ソート)

    for(begin <- 0 until moves.length - 1){
      for(current <- begin + 1 until moves.length){
        val b = moves(begin)
        val c = moves(current)
        if(b.eval < c.eval){
          // 交換
          moves = moves.updated(begin, c)
          moves = moves.updated(current, b)
        }
      }
    }

    return moves
  }

  private def evaluate(board: Board): Int = {
    // later

    return 0
  }
}
