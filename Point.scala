class Point(a: Int, b: Int) {
  val x = a
  val y = b

  require(x >= 1 && x <= 8 && y >= 1 && y <= 8)
  override def toString = {
    val alph = ('a'.toInt + (x-1)).toChar
    val num  = y

    alph.toString + num
  }

  def this() = this(0, 0)
  def this(coord: String) =
    this((coord(0)).toInt - 'a'.toInt + 1, coord(1).toInt - '0'.toInt)
}
