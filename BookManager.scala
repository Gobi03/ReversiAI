import java.io._

object BookManager {
  private final val BOOK_FILE_NAME = "reversi.book"
}

class BookManager {
  val break = new scala.util.control.Breaks

  class Node {
    var child: Option[Node] = None
    var sibling: Option[Node] = None
    var point: Option[Point] = None
  }

  private val Root: Option[Node] = None

  def BookManager(): Unit = {
    Root = new Node()
    Root.point = new Point("f5")

    var fis = null
    try{
      fis = new FileInputStream(BOOK_FILE_NAME)
    }
    catch{
      case e: FileNotFoundException => return ()
    }

    val br = new BufferedReader(new InputStreamReader(fis))

    var line = ""
    try{
      while((line = br.readLine()) != null){
        val book = Vector.empty

        break.breakable{
          for(i <- 0 until line.length if i % 2 == 0){
            var p = null

            
            try{
              p = new Point(line.substring(i))
            }
            catch{
              case e: IllegalArgumentException => break.break
            }
            book = book :+ p
          }
        }
        add(book)
      }
    }
    catch{
      case e: IOException => ()
    }
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

  def add(book: Vector): Unit = {
    val node = Root

    for(i <- 1 until book.length){
      val p = book(i)

      if(node.child == null){
        // 新しい定石手
        node.child = new Node()
        node = node.child
        node.point.x = p.x
        node.point.y = p.y
      }
      else{
        // 兄弟ノードの探索に移る
        node = node.child

        break.breakable{
          while(true){
            // すでにこの手はデータベース中にあり、その枝を見つけた
            if(node.point == p)
              break.break

            // 定石木の新しい枝
            if(node.sibling == null){
              node.sibling = new Node()

              node = node.sibling
              node.point.x = p.x
              node.point.y = p.y
              break.break
            }

            node = node.sibling
          }
        }
      }
    }
  }
}
