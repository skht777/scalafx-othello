/**
  *
  * @author skht777
  *
  */
object State {
  val transStatus: State => State = (s: State) => s.view.status match {
    case Status.Ready => s.copy(status = Status.Active)
    case Status.GameOver => apply
    case _ => s
  }

  val reverse: Point[Int] => State => State = (put: Point[Int]) => (s: State) => {
    val reverse = BitBoard.makeReversedBoard(put)(_, _)

    val (nb, nw) = if (s.view.turn == Turn.Black) reverse(s.black, s.white)
    else reverse(s.white, s.black) swap

    s.copy(nb, nw, s.index + 1)
  }

  val pass: State => State = (s: State) => s.copy(index = s.index + 1)

  def apply: State = State(BitBoard.BLACK, BitBoard.WHITE)
}

sealed case class State(private val black: BitBoard, private val white: BitBoard, private val index: Int = 0, private val status: Status = Status.Ready) {
  private[this] val turn: Turn = if (index % 2 == 0) Turn.Black else Turn.White
  private[this] val legal: BitBoard = if (turn == Turn.Black) BitBoard.makeLegalBoard(black, white) else BitBoard.makeLegalBoard(white, black)

  def view: View = View(black, white, legal, turn, status)

  def current: BitBoard = if (turn == Turn.Black) black else white

  def opponent: BitBoard = if (turn == Turn.Black) white else black
}

sealed case class View(black: BitBoard, white: BitBoard, legal: BitBoard, turn: Turn, status: Status)

sealed trait Status

object Status {

  case object Active extends Status

  case object Ready extends Status

  case object GameOver extends Status

}

sealed trait Turn

object Turn {

  case object Black extends Turn

  case object White extends Turn

}