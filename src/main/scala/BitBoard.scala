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

    direction.shift(trans, false).bits
  }

  def makeLegalBoard(player: BitBoard, opponent: BitBoard): BitBoard = {
    val empty = ~(player | opponent).bits
    var legal: Long = 0L
    Direction.values.foreach(d => legal |= empty & makeTransBoard(player, opponent, d))

    BitBoard(legal)
  }
}

final class Direction private(private[this] val mask: Long, private[this] val shift: BitBoard => BitBoard) {
  def shift(trans: BitBoard, applyMask: Boolean = true): BitBoard = {
    val res = shift(trans)
    if (applyMask) res & mask else res
  }
}

object Direction {
  private val hmask = 0x7e7e7e7e7e7e7e7eL
  private val vmask = 0x00ffffffffffff00L
  private val vhmask = hmask & vmask

  private def lshift(n: Int): BitBoard => BitBoard = l => l << n

  private def rshift(n: Int): BitBoard => BitBoard = l => l >> n

  val Left = new Direction(hmask, lshift(1))

  val Right = new Direction(hmask, rshift(1))

  val Up = new Direction(vmask, lshift(8))

  val Down = new Direction(vmask, rshift(8))

  val UpLeft = new Direction(vhmask, lshift(7))

  val DownLeft = new Direction(vhmask, rshift(7))

  val UpRight = new Direction(vhmask, lshift(9))

  val DownRight = new Direction(vhmask, rshift(9))

  def values = Seq(Left, Right, Up, Down, UpLeft, DownLeft, UpRight, DownRight)
}
