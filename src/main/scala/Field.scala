sealed trait Status

sealed case class View(status: Status)

sealed case class State(status: Status = Status.Ready) {
  def view: View = View(status)
}

/**
  *
  * @author skht777
  *
  */
case class Field(size: Point[Int]) {
  val transStatus = (s: State) => s.status match {
    case Status.Ready => s.copy(status = Status.Active)
    case Status.GameOver => initState
    case _ => s
  }

  def initState = new State()

}

object Status {

  case object Active extends Status

  case object Ready extends Status

  case object GameOver extends Status

}

