class CoordinatesTransformer {
  private var Rotate = 0
  private var Mirror = false

  def CoodinateTransformer(first: Point): Unit = {
    if(first.equals(new Point("d3"))){
      Rotate = 1
      Mirror = true
    }
    else if(first.equals(new Point("c4"))){
      Rotate = 2
    }
    else if(first.equals(new Point("e6"))){
      Rotate = -1
      Mirror = true
    }
  }

  // 座標をf5を開始点とする座標系に正規化する
  def normalize(p: Point): Point = {
    var newp = rotatePoint(p, Rotate)
    if(Mirror)
      newp = mirrorPoint(newp)

    return newp
  }

  // f5 を開始点とする座標を本来の座標に戻す
  def denormalize(p: Point): Point = {
    var newp = new Point(p.x, p.y)
    if(Mirror)
      newp = rotatePoint(newp, -Rotate)
    newp = rotatePoint(newp, -Rotate)

    return newp
  }

  def rotatePoint(old_point: Point, rota: Int): Point = {
    var rotate = rota % 4
    if(rotate < 0)
      rotate += 4

    rotate match{
      case 1 =>
        val x = old_point.y
        val y = Board.BOARD_SIZE - old_point.x + 1
        return new Point(x, y)
      case 2 =>
        val x = Board.BOARD_SIZE - old_point.x + 1
        val y = Board.BOARD_SIZE - old_point.y + 1
        return new Point(x, y)
      case 3 =>
        val x = Board.BOARD_SIZE - old_point.y + 1
        val y = old_point.x
        return new Point(x, y)
      case _ =>
        val x = old_point.x
        val y = old_point.y
        return new Point(x, y)
    }
  }

  def mirrorPoint(point: Point): Point =
    new Point(BOARD_SIZE - point.x + 1, point.y)
}
