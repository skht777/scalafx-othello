/**
  *
  * @author skht777
  *
  */
case class Field() {
  val transStatus = (s: State) => s.view.status match {
    case Status.Ready => s.copy(status = Status.Active)
    case Status.GameOver => initState
    case _ => s
  }

  val reverse = (put: Point[Int]) => (s: State) => s.update(put)

  def initState = State(BitBoard.BLACK, BitBoard.WHITE)
}

sealed case class State(private val black: BitBoard, private val white: BitBoard, private val index: Int = 0, private val status: Status = Status.Ready) {
  private[this] val turn: Turn = if (index % 2 == 0) Turn.Black else Turn.White
  private[this] val legal: BitBoard = if (turn == Turn.Black) BitBoard.makeLegalBoard(black, white) else BitBoard.makeLegalBoard(white, black)

  def view: View = View(black, white, legal, turn, status)

  def update(put: Point[Int]): State = {
    val (nb, nw) = if (turn == Turn.Black)
      BitBoard.makeReversedBoard(put)(black, white)
    else BitBoard.makeReversedBoard(put)(white, black) swap

    copy(nb, nw, index + 1)
  }
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

