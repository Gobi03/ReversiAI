class PerfectEvaluator extends Evaluator {
  def evaluate(board: Board): Unit = {
    val discdiff =
      board.getCurrentColor().num
    * (board.countDisc(Disc.BLACK) - board.countDisc(Disc.WHITE))

    return discdiff
  }
}
