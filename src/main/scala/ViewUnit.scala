/**
  *
  * @author skht777
  *
  */
case class ViewUnit() {
  private[this] var state = State.apply

  def view: View = state.view

  def reverse(put: Point[Int]): Unit = state = State.reverse(put)(state)

  def pass(): Unit = state = State.pass(state)
}

