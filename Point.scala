class Point(coord: String) {
  require(coord != null && coord.length == 2)
  val x = coord(0).toInt - 'a'.toInt + 1
  val y = coord(1).toInt - '1'.toInt + 1

  require(x >= 0 && x <= 9 && y >= 0 && y <= 9)
  override def toString = coord

  def this() = this("`0")
  def this(a: Int, b: Int) =
    this(('a'.toInt + a - 1).toChar.toString + ('1'.toInt + b - 1).toChar.toString)
}
