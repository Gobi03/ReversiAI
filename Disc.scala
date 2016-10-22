object Disc {
  sealed abstract class Color(val num: Int)
  case object BLACK extends Color(1)
  case object EMPTY extends Color(0)
  case object WHITE extends Color(-1)
  case object WALL  extends Color(2)

  def colorOfInt(n: Int): Color = {
    require(n >= -1 && n <= 2)

    n match{
      case 1  => BLACK
      case 0  => EMPTY
      case -1 => WHITE
      case _  => WALL
    }
  }

  def revColor(color: Color): Color =
    color match{
      case BLACK => WHITE
      case WHITE => BLACK
      case  _    => throw new Exception("not color")
    }
}


class Disc
  (override val x: Int, override val y: Int, val color: Disc.Color)
    extends Point {
  def this() = this(0, 0, Disc.EMPTY)
}
