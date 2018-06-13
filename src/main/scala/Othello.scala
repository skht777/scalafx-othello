import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafxml.core.macros.sfxml
import scalafxml.core.{FXMLView, NoDependencyResolver}

/**
  *
  * @author skht777
  *
  */
object Othello extends JFXApp {
  stage = new PrimaryStage {
    title = "othello"
    scene = new Scene(FXMLView(getClass.getResource("root.fxml"), NoDependencyResolver))
  }
}

@sfxml
class fieldController(private val canvas: Canvas) {
  private[this] val unit = ViewUnit()
  private[this] val blockSize = canvas.getWidth / 8
  private[this] val gc: GraphicsContext = jfxGraphicsContext2sfx(canvas getGraphicsContext2D)
  gc.font = Font.font(40)
  drawView()

  def operate(e: MouseEvent): Unit = {
    val se = jfxMouseEvent2sfx(e)
    val p = Point(se.x, se.y) map (n => Math.floor(n / blockSize) toInt)
    if (unit.view.legal.valid(p)) {
      unit.reverse(p)
      drawView()
    }
  }

  private[this] def drawView(): Unit = {
    def strokeLine(x: Double, y: Double, w: Double, h: Double): Unit = gc.strokeLine(x, y, x + w, y + h)

    def strokeLines(w: Int, h: Int, size: Double, exX: Double = 0, exY: Double = 0): Unit = {
      gc.stroke = Color.Black
      0 to w map (_ * size + exX) foreach (strokeLine(_, exY, 0, size * h))
      0 to h map (_ * size + exY) foreach (strokeLine(exX, _, size * w, 0))
    }

    def drawStone(inverse: Boolean, opacity: Double = 1)(pos: Point[Int]): Unit = {
      val margin = 20.0
      val p = pos.map(_ * blockSize) + Point(margin, margin) * 0.5
      val r = blockSize - margin
      gc.fill = if (inverse) Color.Black.opacity(opacity) else Color.White.opacity(opacity)
      gc.fillOval(p.x, p.y, r, r)
    }

    gc.fill = Color.Green
    gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
    strokeLines(8, 8, blockSize)
    val black = BitBoard.toPoint(unit.view.black)
    val white = BitBoard.toPoint(unit.view.white)
    black foreach drawStone(inverse = true)
    white foreach drawStone(inverse = false)

    gc.fill = Color.Gray
    gc.fillRect(200, 850, 400, 100)
    gc.fill = Color.White
    gc.fillOval(250, 870, 60, 60)
    gc.fill = Color.Black
    gc.fillOval(490, 870, 60, 60)
    gc.fillText(white.length.toString + " - " + black.length.toString, 340, 915)
    gc.fillRect(if (unit.view.turn == Turn.White) 250 else 490, 940, 60, 10)

    val legal = BitBoard.toPoint(unit.view.legal)
    // legal foreach drawStone(unit.view.turn != Turn.White, 0.5)
    if (black.length + white.length != 64 && legal.isEmpty) {
      unit.pass()
      drawView()
    } else if (unit.view.turn == Turn.White) {
      unit.compute()
      drawView()
    }
  }
}
