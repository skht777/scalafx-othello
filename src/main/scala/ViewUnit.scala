/**
  *
  * @author skht
  *
  */
case class ViewUnit(size: Point[Int]) {
  private[this] val field = Field(size)
  private[this] var state = field.initState

  def view = state.view

  def reverse(put: Point[Int]) = state = field.reverse(put)(state)

  def status() = field.transStatus

}

