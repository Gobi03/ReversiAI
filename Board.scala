object Board {
  val BOARD_SIZE = 8
  val MAX_TURNS = 60

  // bit masks for direction
  val NONE         = 0
  val UPPER        = 1
  val UPPER_LEFT   = 2
  val LEFT         = 4
  val LOWER_LEFT   = 8
  val LOWER        = 16
  val LOWER_RIGHT  = 32
  val RIGHT        = 64
  val UPPER_RIGHT  = 128
}

class Board() {
  import Board._

  private val RawBoard =
    Array.ofDim[Disc.Color](BOARD_SIZE+2, BOARD_SIZE+2)
  private var Turns = 0
  private var CurrentColor: Disc.Color = Disc.BLACK

  private var UpdateLog =
    Vector.empty[scala.collection.immutable.Vector[Disc]]

  /* ターン、座標ごとのビットマスクが入る */
  private var MovableDir =
    Array.ofDim[Int](MAX_TURNS+1, BOARD_SIZE+2, BOARD_SIZE+2)
  /* 打てる位置の座標の Vector */
  // Array[Vector[Disc]]
  private val MovablePos =
    Array.fill(MAX_TURNS+1)(Vector.empty[Disc])

  // stone num of each colors
  private val Discs = new ColorStorage()

  /* ボードをゲーム開始直後の状態にする。 */
  def init(): Unit = {
    // 全マスを空きマスに設定
    for(x <- 1 to BOARD_SIZE)
      for(y <- 1 to BOARD_SIZE)
        RawBoard(x)(y) = Disc.EMPTY

    // 壁の設定
    for(y <- 0 until BOARD_SIZE + 2){
      RawBoard(0)(y) = Disc.WALL
      RawBoard(BOARD_SIZE+1)(y) = Disc.WALL
    }
    for(x <- 0 until BOARD_SIZE + 2){
      RawBoard(0)(x) = Disc.WALL
      RawBoard(BOARD_SIZE+1)(x) = Disc.WALL
    }

    // 初期配置
    RawBoard(4)(4) = Disc.WHITE
    RawBoard(5)(5) = Disc.WHITE
    RawBoard(4)(5) = Disc.BLACK
    RawBoard(5)(4) = Disc.BLACK

    // 石数の初期設定
    Discs.set(Disc.BLACK, 2)
    Discs.set(Disc.WHITE, 2)
    Discs.set(Disc.EMPTY, BOARD_SIZE*BOARD_SIZE - 4)

    Turns = 0
    CurrentColor = Disc.BLACK

    // update をすべて消去
    UpdateLog = Vector.empty

    initMovable()
  }
  /* 石を打つ処理 */
  /* 打てない位置だったらfalseを返す */
  def move(point: Point): Boolean = {
    if(point.x <= 0 || point.x > BOARD_SIZE)
      false
    else if(point.y <= 0 || point.y > BOARD_SIZE)
      false
    else if(MovableDir(Turns)(point.x)(point.y) == NONE){
      println("hoge")
      false
    }
    else{
      flipDiscs(point)

      Turns += 1
      CurrentColor = Disc.revColor(CurrentColor)

      initMovable()

      return true
    }
  }
  /* 直前の一手を元に戻す。 */
  def undo(): Boolean = {
    if(Turns == 0)
      false
    else{
      CurrentColor = Disc.revColor(CurrentColor)
      // 最新のターンの盤面変化の情報
      val update = UpdateLog(UpdateLog.length - 1)
      UpdateLog = UpdateLog.dropRight(1)

      if(update.isEmpty){  // 前回がパス
        // MovablePos と MovableDir を再構築
        MovablePos(Turns) = Vector.empty
        for(x <- 1 to BOARD_SIZE)
          for(y <- 1 to BOARD_SIZE)
            MovableDir(Turns)(x)(y) = NONE
      }
      else{  // 前回はパスでない
        Turns -= 1

        // 石を元に戻す
        val tmpP = update(0)
        RawBoard(tmpP.x)(tmpP.y) = Disc.EMPTY
        val revColor = Disc.revColor(CurrentColor)
        for(i <- 1 until update.length){
          println(i)
          val p = update(i)
          RawBoard(p.x)(p.y) = revColor
        }

        // 石数の更新
        val discdiff = update.length
        Discs.set(CurrentColor, Discs.get(CurrentColor) - discdiff)
        Discs.set(revColor,     Discs.get(revColor) + (discdiff - 1))
        Discs.set(Disc.EMPTY, Discs.get(Disc.EMPTY) + 1)
      }

      true
    }
  }
  /* パスする。成功したら true が返る。パスできない場合は false が返る */
  def pass(): Boolean = {
    if(MovablePos(Turns).length != 0)  // 打つ手がある
      false
    else if(isGameOver())
      false
    else{
      CurrentColor = Disc.revColor(CurrentColor)
      UpdateLog = UpdateLog :+ Vector.empty
      initMovable()

      true
    }
  }
  /* 座標 p の色を返す */
  def getColor(p: Point): Disc.Color = RawBoard(p.x)(p.y)
  /* 現在の手番を返す */
  def getCurrentColor(): Disc.Color = CurrentColor
  def getTurns(): Int = Turns
  /* ゲームが終了していれば true を、終了していなければ false を返す。 */
  def isGameOver(): Boolean = {
    if(Turns == MAX_TURNS)
      true
    else if(MovablePos(Turns).length != 0)
      false
    else{  // pass の場合打てるか
      var res = true
      val revColor = Disc.revColor(CurrentColor)
      for(x <- 1 to BOARD_SIZE)
        for(y <- 1 to BOARD_SIZE)
          if(checkMobility(new Disc(x, y, revColor)) != NONE)
            res = false

      res
    }
  }
  def countDisc(color: Disc.Color): Int = Discs.get(color)
  /* 石を打てる座標が並んだ vector を返す。 */
  def getMovablePos(): scala.collection.immutable.Vector[Disc] =
    MovablePos(Turns)
  /* 直前の手で打った石と裏返した石が並んだ vector を返す */
  def getUpdate(): scala.collection.immutable.Vector[Disc] = {
    if(UpdateLog.isEmpty)
      Vector.empty
    else
      UpdateLog(UpdateLog.length - 1)
  }

  // ある位置に石を打てるかどうか
  private def checkMobility(disc: Disc): Int = {
    if(RawBoard(disc.x)(disc.y) != Disc.EMPTY)  // stone exists
      NONE
    else {
      // returned bit-mask
      var res = NONE

      val anti = Disc.colorOfInt(-disc.color.num)

      // vol is (dx, dy)  -1 <= dx, dy <= 1
      // checkdir is UPPER or LOWER or ...
      def check(vol: (Int, Int), checkdir: Int): Unit = {
        val (vx, vy) = vol
        // 隣が自分と別の色だった場合
        if(RawBoard(disc.x + vx)(disc.y + vy) == anti){
          var x = disc.x + 2*vx
          var y = disc.y + 2*vy
          // 同じ色か壁に当たるまで
          while(RawBoard(x)(y) == anti){
            x += vx
            y += vy
          }
          if(RawBoard(x)(y) == disc.color)
            res |= checkdir
        }
      }

      check((0, -1), UPPER)
      check((0, 1), LOWER)
      check((-1, 0), LEFT)
      check((1, 0), RIGHT)
      check((1, -1), UPPER_RIGHT)
      check((-1, -1), UPPER_LEFT)
      check((1, 1), LOWER_RIGHT)
      check((-1, 1), LOWER_LEFT)

      res
    }
  }
  /* MovablePos(Turns)とMovableDir(Turns)を再計算する */
  private def initMovable(): Unit = {
    MovablePos(Turns) = Vector.empty

    for(y <- 1 to BOARD_SIZE){
      for(x <- 1 to BOARD_SIZE){
        val disc = new Disc(x, y, CurrentColor)
        val dir = checkMobility(disc)
        if(dir != NONE){
          // 置ける場所
          MovablePos(Turns) = MovablePos(Turns) :+ disc
        }
        MovableDir(Turns)(x)(y) = dir
      }
    }
  }
  /* pointで指定された位置に石を打ち、挟み込めるすべての石を裏返す。 */
  /* 「打った石」と「裏返した石」をUpdateLogに挿入する */
  private def flipDiscs(point: Point): Unit = {
    val dir = MovableDir(Turns)(point.x)(point.y)

    var update = Vector.empty[Disc]

    RawBoard(point.x)(point.y) = CurrentColor
    update = update :+ (new Disc(point.x, point.y, CurrentColor))

    // vol is (dx, dy)  -1 <= dx, dy <= 1
    // checkdir is UPPER or LOWER or ...
    def check(vol: (Int, Int), checkdir: Int): Unit = {
      val vx = vol._1; val vy = vol._2
      if((dir & checkdir) != NONE){
        var x = point.x + vx
        var y = point.y + vy

        while(RawBoard(x)(y) != CurrentColor){
          RawBoard(x)(y) = CurrentColor
          update = update :+ (new Disc(x, y, CurrentColor))
          x += vx; y += vy
        }
      }
    }

    check((0, -1), UPPER)
    check((0, 1), LOWER)
    check((-1, 0), LEFT)
    check((1, 0), RIGHT)
    check((1, -1), UPPER_RIGHT)
    check((-1, -1), UPPER_LEFT)
    check((1, 1), LOWER_RIGHT)
    check((-1, 1), LOWER_LEFT)

    val discdiff = update.length

    val revColor = Disc.revColor(CurrentColor)
    Discs.set(CurrentColor, Discs.get(CurrentColor) + discdiff)
    Discs.set(revColor,     Discs.get(revColor) - (discdiff - 1))
    Discs.set(Disc.EMPTY, Discs.get(Disc.EMPTY) - 1)

    UpdateLog = UpdateLog :+ update
  }

  // /// Section 5
  // // Vector の型とか調整
  // def getHistory(): Vector = {
  //   var history = Vector.empty
  //   for(i <- 0 until UpdateLog.size()){
  //     val update = UpdateLog(i)
  //     if(!update.isEmpty)
  //       history = history :+ add(update(0))
  //   }
  // }
}
