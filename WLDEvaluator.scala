class WLDEvaluator extends Evaluator {
  final val WIN = 1
  final val DRAW = 0
  final val LOSE = -1

  def evaluate(board: Board): Int = {
    val discdiff =
      board.getCurrentColor().num
      * (board.countDisc(Disc.BLACK) - board.countDisc(Disc.WHITE))

    if(discdiff > 0)
      return WIN
    else if(discdiff < 0)
      return LOSE
    else
      return DRAW
  }
}
