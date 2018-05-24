/**
  *
  * @author skht777
  *
  */
case class Field() {
  val transStatus = (s: State) => s.status match {
    case Status.Ready => s.copy(status = Status.Active)
    case Status.GameOver => initState
    case _ => s
  }

  val reverse = (put: Point[Int]) => (s: State) => s.update(put)

  def initState = State(BitBoard.BLACK, BitBoard.WHITE)
}

sealed case class State(black: BitBoard, white: BitBoard, index: Int = 0, status: Status = Status.Ready) {
  private[this] val legal: BitBoard = if (index % 2 == 0) BitBoard.makeLegalBoard(black, white) else BitBoard.makeLegalBoard(white, black)

  def view: View = View(black, white, legal, status)

  def update(put: Point[Int]): State = {
    val (nb, nw) = index % 2 match {
      case 0 => BitBoard.makeReversedBoard(put)(black, white)
      case 1 => BitBoard.makeReversedBoard(put)(white, black) swap
    }
    copy(nb, nw, index + 1)
  }
}

sealed case class View(black: BitBoard, white: BitBoard, legal: BitBoard, status: Status)

sealed trait Status

object Status {

  case object Active extends Status

  case object Ready extends Status

  case object GameOver extends Status

}

