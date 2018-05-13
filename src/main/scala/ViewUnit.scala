/**
  *
  * @author skht
  *
  */
case class ViewUnit(size: Point[Int]) {
  private[this] val field = Field(size)
  private[this] var state = field.initState

  def view = state.view

  def trans(s: State => State) = state = s(state)

  def status() = field.transStatus

}

