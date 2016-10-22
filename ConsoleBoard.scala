class ConsoleBoard extends Board {
  def printing() {
    println("  ａｂｃｄｅｆｇｈ")
    for(y <- 1 to 8){
      print(" " + y)
      for(x <- 1 to 8){
        getColor(new Point(x, y)) match {
          case Disc.BLACK => print("●")
          case Disc.WHITE => print("◯")
          case Disc.EMPTY => print("・")
          case Disc.WALL  =>
            throw new Exception("Shouldn't be Disc.WALL")
        }
      }
      println()
    }
  }
}
