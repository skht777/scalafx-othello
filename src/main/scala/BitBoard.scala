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

  def makeReverseBoard(pos: BitBoard, player: BitBoard, opponent: BitBoard): BitBoard = {
    var res = ZERO
    Direction.values.foreach(d => {
      var tmp = ZERO
      var mask = d.reverseShift(pos)
      while (mask != ZERO && (mask & opponent) != ZERO) {
        tmp |= mask
        mask = d.reverseShift(mask)
      }
      if ((mask & player) != ZERO) {
        res |= tmp
      }
    })

    res
  }

  def makeReversedBoard(put: Point[Int])(player: BitBoard, opponent: BitBoard): (BitBoard, BitBoard) = {
    val pos = apply(put)
    val res = makeReverseBoard(pos, player, opponent)
    (player ^ (pos | res), opponent ^ res)
  }

  private def makeTransBoard(player: BitBoard, opponent: BitBoard, direction: Direction): BitBoard = {
    var trans = opponent & direction.legalShift(player)
    (1 to 5) foreach (_ => trans |= opponent & direction.legalShift(trans))

    direction.legalShift(trans, false)
  }

  def makeLegalBoard(player: BitBoard, opponent: BitBoard): BitBoard = {
    val empty = ~(player | opponent).bits
    var legal = ZERO
    Direction.values.foreach(d => legal |= makeTransBoard(player, opponent, d) & empty)

    legal
  }
}

final case class Direction private(private val reverseMask: Long, private val legalMask: Long, private val shift: BitBoard => BitBoard) {
  private def shift(trans: BitBoard, mask: Long, applyMask: Boolean = true): BitBoard = {
    val res = shift(trans)
    if (applyMask) res & mask else res
  }

  def legalShift(trans: BitBoard, applyMask: Boolean = true): BitBoard = shift(trans, legalMask, applyMask)

  def reverseShift(trans: BitBoard): BitBoard = shift(trans, reverseMask)
}

object Direction {
  private val lmask = 0xfefefefefefefefeL
  private val rmask = 0x7f7f7f7f7f7f7f7fL
  private val umask = 0xffffffffffffff00L
  private val dmask = 0x00ffffffffffffffL
  private val hmask = lmask & rmask
  private val vmask = umask & dmask
  private val vhmask = hmask & vmask
  private val lshift = (n: Long) => (l: BitBoard) => l << n
  private val rshift = (n: Long) => (l: BitBoard) => l >> n

  val Left = Direction(lmask, hmask, lshift(1))
  val Right = Direction(rmask, hmask, rshift(1))
  val Up = Direction(umask, vmask, lshift(8))
  val Down = Direction(dmask, vmask, rshift(8))
  val UpLeft = Direction(umask & lmask, vhmask, lshift(9))
  val DownLeft = Direction(dmask & lmask, vhmask, rshift(7))
  val UpRight = Direction(umask & rmask, vhmask, lshift(7))
  val DownRight = Direction(dmask & rmask, vhmask, rshift(9))

  def values = Seq(Left, Right, Up, Down, UpLeft, DownLeft, UpRight, DownRight)
}
