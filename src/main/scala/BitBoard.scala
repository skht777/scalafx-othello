/**
  *
  * @author skht777
  *
  */
sealed case class BitBoard(private val bits: Long) {
  def &(bit: Long): BitBoard = copy(bits & bit)

  def &(board: BitBoard): BitBoard = this & board.bits

  def |(bit: Long): BitBoard = copy(bits | bit)

  def |(board: BitBoard): BitBoard = this | board.bits

  def >>(bit: Long): BitBoard = copy(bits >> bit)

  def <<(bit: Long): BitBoard = copy(bits << bit)

  def ^(bit: Long): BitBoard = copy(bits ^ bit)

  def ^(bit: BitBoard): BitBoard = this ^ bit.bits

  def check(put: Point[Int]): Boolean = (bits >> (put.x + put.y * 8) & 1) != 0
}

object BitBoard {
  private val ZERO = BitBoard(0L)
  val BLACK = BitBoard(0x0000000810000000L)
  val WHITE = BitBoard(0x0000001008000000L)

  private def apply(put: Point[Int]): BitBoard = BitBoard(1L << (put.x + put.y * 8))

  def makeReversedBoard(put: Point[Int])(player: BitBoard, opponent: BitBoard): (BitBoard, BitBoard) = {
    val pos = apply(put)
    var res = 0L
    Direction.values.foreach(d => {
      var tmp = 0L
      var mask = d.shift(pos)
      while (mask != ZERO && (mask & opponent) != ZERO) {
        tmp |= mask.bits
        mask = d.shift(mask)
      }
      if ((mask & player) != ZERO) {
        res |= tmp
      }
    })

    (player ^ (pos | res), opponent ^ res)
  }

  private def makeTransBoard(player: BitBoard, opponent: BitBoard, direction: Direction): Long = {
    var trans = opponent & direction.shift(player)
    (0 to 5) foreach (_ => trans = trans | (opponent & direction.shift(trans)))
    direction.shift(trans).bits
  }

  def makeLegalBoard(player: BitBoard, opponent: BitBoard): BitBoard = {
    val empty = ~(player | opponent).bits
    var legal: Long = 0L
    Direction.values.foreach(d => legal |= empty & makeTransBoard(player, opponent, d))

    BitBoard(legal)
  }
}

sealed trait Direction {
  def shift(trans: BitBoard): BitBoard
}

object Direction {
  private val hmask = 0x7e7e7e7e7e7e7e7eL
  private val vmask = 0x00ffffffffffff00L
  private val vhmask = hmask & vmask

  private def lshift(n: Int): BitBoard => BitBoard = l => l << n

  private def rshift(n: Int): BitBoard => BitBoard = l => l >> n

  private def shift(trans: BitBoard, mask: Long, shift: BitBoard => BitBoard) = shift(trans) & mask

  case object Left extends Direction {
    override def shift(trans: BitBoard) = Direction.shift(trans, hmask, lshift(1))
  }

  case object Right extends Direction {
    override def shift(trans: BitBoard) = Direction.shift(trans, hmask, rshift(1))
  }

  case object Up extends Direction {
    override def shift(trans: BitBoard) = Direction.shift(trans, vmask, lshift(8))
  }

  case object Down extends Direction {
    override def shift(trans: BitBoard) = Direction.shift(trans, vmask, rshift(8))
  }

  case object UpLeft extends Direction {
    override def shift(trans: BitBoard) = Direction.shift(trans, vhmask, lshift(7))
  }

  case object DownLeft extends Direction {
    override def shift(trans: BitBoard) = Direction.shift(trans, vhmask, rshift(7))
  }

  case object UpRight extends Direction {
    override def shift(trans: BitBoard) = Direction.shift(trans, vhmask, lshift(9))
  }

  case object DownRight extends Direction {
    override def shift(trans: BitBoard) = Direction.shift(trans, vhmask, rshift(9))
  }

  def values = Seq(Left, Right, Up, Down, UpLeft, DownLeft, UpRight, DownRight)
}
