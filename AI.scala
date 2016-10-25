abstract class AI {
  def move(board: Board): Unit

  val presearch_depth = 3
  val normal_depth = 5
  val wld_depth = 15
  val perfect_depth = 13
}
