import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafxml.core.macros.sfxml
import scalafxml.core.{FXMLView, NoDependencyResolver}

@sfxml
class fieldController(private val canvas: Canvas) {
  private[this] val unit = ViewUnit(Point(8, 8))
  private[this] val blockSize = canvas.getWidth / unit.size.x
  private[this] val gc: GraphicsContext = jfxGraphicsContext2sfx(canvas getGraphicsContext2D)
  drawView()

  def operate(e: MouseEvent): Unit = {
    val se = jfxMouseEvent2sfx(e)
    val p = Point(se.x, se.y) map (n => Math.floor(n / blockSize) toInt)
    drawBlock(p, blockSize, false)

  }

  private[this] def transAndDraw(trans: State => State) = {
    unit.trans(trans)
    drawView()
  }

  private[this] def drawView() = {
    def strokeLine(x: Double, y: Double, w: Double, h: Double) = gc.strokeLine(x, y, x + w, y + h)

    def strokeLines(w: Int, h: Int, size: Double, exX: Double = 0, exY: Double = 0) = {
      gc.stroke = Color.Black
      0 to w map (i => i * size) foreach (x => strokeLine(exX + x, exY, 0, size * h))
      0 to h map (i => i * size) foreach (y => strokeLine(exX, exY + y, size * w, 0))
    }

    gc.fill = Color.Green
    gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
    strokeLines(unit.size.x, unit.size.y, blockSize)
    drawBlock(Point(3, 3), blockSize, false)
    drawBlock(Point(4, 3), blockSize, true)
    drawBlock(Point(3, 4), blockSize, true)
    drawBlock(Point(4, 4), blockSize, false)

    gc.stroke = Color.White
    //
    /*unit.view.status match {
      case Status.Ready => gc.strokeText("press the enter", size / 2, size / 2)
      case Status.GameOver => gc.strokeText("game over", size / 2, size / 2)
      case _ =>
    }*/
  }

  private[this] def drawBlock(pos: Point[Int], size: Double, inverse: Boolean) = {
    val margin = 20.0
    val p = pos.map(n => n.toDouble) * size + Point(margin, margin) * 0.5
    val r = size - margin
    gc.fill = if (inverse) Color.Black else Color.White
    gc.stroke = Color.Black
    gc.fillOval(p.x, p.y, r, r)
  }
}

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
