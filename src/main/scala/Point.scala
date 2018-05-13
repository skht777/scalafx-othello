/**
  *
  * @author skht777
  *
  */
case class Point[T](x: T, y: T)(implicit num: Numeric[T]) {

  import num._

  def move[U <: T](x: U, y: U): Point[T] = copy(this.x + x, this.y + y)

  def move[U >: T](x: U, y: U)(implicit u: Numeric[U]): Point[U] = this + Point(x, y)

  def +[U >: T](other: Point[U])(implicit u: Numeric[U]): Point[U] = copy[U](x, y) + other

  def +[U <: T](other: Point[U]): Point[T] = copy(x + other.x, y + other.y)

  def *[U](scalar: U)(implicit ev$1: U => T): Point[T] = map(_ * scalar)

  def map[U](func: T => U)(implicit u: Numeric[U]): Point[U] = copy(func(x), func(y))
}