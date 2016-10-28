class MidEvaluator extends Evaluator {
  class EdgeParam {
    var stable: Byte = 0  // 確定石の個数
    var wing: Byte = 0    // ウイングの個数
    var mountain: Byte = 0  // 山の個数
    var Cmove: Byte = 0     // 危険なC打ちの個数

    def add(src: EdgeParam): EdgeParam = {
      stable += src.stable
      wing += src.wing
      mountain += src.mountain
      Cmove += src.Cmove

      return this
    }

    // 代入演算子の代わり
    def set(e: EdgeParam): Unit = {
      stable = e.stable
      wing += src.wing
      mountain += src.mountain
      Cmove += src.Cmove

      return this
    }
  }

  class CornerParam {
    var corner: Byte = 0
    var Xmove: Byte = 0
  }

  class EdgeStat {
    private val data = new Array[EdgeParam](3)

    private def EdgeStat(): Unit = {
      for(i <- 0 until 3)
        data(i) = new EdgeParam()
    }

    def add(e: EdgeStat): Unit = {
      for(i <- 0 until 3)
        data(i).add(e.add(i))
    }

    def get(color: Disc.Color): EdgeParam = data(color.num + 1)
  }

  class CornerStat {
    private val data = new Array[CornerParam](3)
    def CornerStat(): Unit = {
      for(i <- 0 until 3)
        data(i) = new CornerParam
    }
    def get(color: Disc.Color): CornerParam = data(color.num + 1)
  }

  // 重み係数を規定する構造体
  class Weight {
    val mobility_w = 67
    val library_w  = -13
    val stable_w   = 101
    val wing_w     = -308
    val Xmove_w    = -449
    val Cmove_w    = -552
  }

  private final val TABLE_SIZE = 6561
  private val EdgeTable = new EdgeStat(TABLE_SIZE)
  private var TableInit = false

  private def MidEvaluator(): Unit = {
    if(!TableInit){
      // 初回起動にテーブルを生成
      val line = new Array[Int](Board.BOARD_SIZE)
      generateEdge(line, 0)

      TableInit = true
    }

    // 重み係数の設定(全局面共通)
    val EvalWeight = new Weight
  }

  private def evaluate(board: Board) = {
    // 辺の評価
    val edgestat = EdgeTable(idxTop(board))
    edgestat.add(EdgeTable(idxBottom(board)))
    edgestat.add(EdgeTable(idxRight(board)))
    edgestat.add(EdgeTable(idxLeft(board)))

    // 隅の評価
    val cornerstat = evalCorner(board)

    // 確定石に関して、隅の石を数えてしまっているので補正。
    edgestat.get(Disc.BLACK).stable
                    -= cornerstat.get(Disc.BLACK).corner
    edgestat.get(Disc.WHITE).stable
                    -= cornerstat.get(Disc.WHITE).corner

    // パラメータの線形結合
    var result =
      edgestat.get(Disc.BLACK).stable * EvalWeight.stable_w
    - edgestat.get(Disc.WHITE).stable * EvalWeight.stable_w
    + edgestat.get(Disc.BLACK).wing * EvalWeight.wing_w
    - edgestat.get(Disc.WHITE).wing * EvalWeight.wing_w
    + edgestat.get(Disc.BLACK).Xmove * EvalWeight.Xmove_w
    - edgestat.get(Disc.WHITE).Xmove * EvalWeight.Xmove_w
    + edgestat.get(Disc.BLACK).Cmove * EvalWeight.Cmove_w
    - edgestat.get(Disc.WHITE).Cmove * EvalWeight.Cmove_w

    // 開放度・着手可能手数の評価
    if(EvalWeight.library_w != 0){
      val liberty: ColorStorage = countLiberty(board)
      result += liberty.get(Disc.BLACK) * EvalWeight.liberty_w
      result -= liberty.get(Disc.WHITE) * EvalWeight.liberty_w
    }

    // 現在の手番の色についてのみ、着手可能手数を数える
    result +=
      board.getCurrentColor()
    * board.getMovablePos().length
    * EvalWeight.mobility_w

    return board.getCurrentColor().num * result
  }

  private def generateEdge(edge: Array[Int], count: Int): Unit = {
    if(count == Board.BOARD_SIZE){
      // このパターンは完成したので、局面のカウント
      val stat = new EdgeStat()

      stat.get(Disc.BLACK).set(evalEdge(edge, Disc.BLACK))
      stat.get(Disc.WHITE).set(evalEdge(edge, Disc.WHITE))

      EdgeTable(idxLine(edge)) = stat

      return ()
    }
    else{
      // 再帰的にすべてのパターンを網羅
      
    }
  }
}
