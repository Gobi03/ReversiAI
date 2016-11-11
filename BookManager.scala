object BookManager {
  private final val BOOK_FILE_NAME = "reversi.book"
}

class BookManager {
  class Node {
    var child: Option[Node] = None
    var sibling: Option[Node] = None
    var point: Option[Point] = None
  }

  private val Root: Option[Node] = None

  def BookManager(): Unit = {

  }

  def find(board: Board): Vector = {
        val node = Root
    val history = board.getHistory()

    if(history.isEmpty)
      return board.getMovablePos()

    val first = history(0)
    val transformer = new CoordinatesTransformer(first)

    // 座標を変換して f5 から始まるようにする
    val normalized = Vector.empty
    for(i <- 0 until history.length){
      val p = history(i)

      normalized.add(transformer.normalize(p))
    }

    val break = new scala.util.control.Breaks
    
    // 現在までの棋譜リストと定石の対応を取る
    for(i <- 1 until normalized.length){
      val p = normalized(i)

      node = node.child
      break.breakable{
        while(node != None){
          if(node.point == p)
            break.break
          else
            node = node.sibling
        }
      }
      if(node == None){
        // 定石を外れている
        return board.getMovablePos()
      }
    }

    // 履歴と定石の終わりが一致していた場合
    if(node.child == None)
      return board.getMovablePos

    val next_move = getNextMove(node)

    // 座標を元の形に変換する
    next_move = transformer.denormalize(next_move)
    val v = Vector.empty
    v.add(next_move)

    return v
  }

  def getNextMove(node: Node): Point = {
    val candidates = Vector.empty

    var p = node.child
    while(p != null){
      candidates.add(p.point)

      p = p.sibling
    }

    val index = (Math.random() * candidates.length).toInt
    val point = candidates(index)

    return new Point(point.x, point.y)
  }

  def add(book: Vector): Unit
}
