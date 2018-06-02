import scala.annotation.tailrec

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
    @tailrec
    def recursive(head: BitBoard, tails: Seq[BitBoard] = Seq())(d: Direction): BitBoard = {
      if ((head & opponent) != ZERO) recursive(d.reverseShift(head), tails :+ head)(d)
      else if ((head & player) != ZERO) tails.foldLeft(ZERO)(_ | _) else ZERO
    }

    Direction.values.map(d => recursive(d.reverseShift(pos))(d)).foldLeft(ZERO)(_ | _)
  }

  def makeReversedBoard(put: Point[Int])(player: BitBoard, opponent: BitBoard): (BitBoard, BitBoard) = {
    val pos = apply(put)
    val res = makeReverseBoard(pos, player, opponent)
    (player ^ (pos | res), opponent ^ res)
  }

  private def makeTransBoard(player: BitBoard, opponent: BitBoard, direction: Direction): BitBoard = {
    def trans(board: BitBoard) = opponent & direction.legalShift(board)

    val res = (1 to 5).foldLeft(trans(player)) { (b, _) => b | trans(b) }

    direction.legalShift(res, false)
  }

  def makeLegalBoard(player: BitBoard, opponent: BitBoard): BitBoard = {
    val empty = ~(player | opponent).bits

    Direction.values.map(makeTransBoard(player, opponent, _) & empty).foldLeft(ZERO)(_ | _)
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
