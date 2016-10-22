// 石の数を保存
class ColorStorage {
  private val data = new Array[Int](3)

  def get(color: Disc.Color): Int = {
    require(color != Disc.WALL)
    data(color.num + 1)
  }

  def set(color: Disc.Color, value: Int): Unit = {
    require(color != Disc.WALL)
    data(color.num + 1) = value
  }
}
