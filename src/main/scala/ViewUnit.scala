/**
  *
  * @author skht777
  *
  */
case class ViewUnit() {
  private[this] var state = State.apply
  private[this] val cpu = AI()

  def view: View = state.view

  def reverse(put: Point[Int]): Unit = state = State.reverse(put)(state)

  def pass(): Unit = state = State.pass(state)

  def compute(): Unit = state = State.reverse(cpu.calculate(state))(state)
}

