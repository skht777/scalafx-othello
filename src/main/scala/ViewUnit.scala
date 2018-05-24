/**
  *
  * @author skht
  *
  */
case class ViewUnit(size: Point[Int]) {
  private[this] val field = Field()
  private[this] var state = field.initState

  def view: View = state.view

  def reverse(put: Point[Int]): Unit = state = field.reverse(put)(state)
}

