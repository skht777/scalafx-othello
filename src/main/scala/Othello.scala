import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.concurrent.Task
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.{Color, PaintIncludes}
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
  private[this] var computed: Boolean = false

  private[this] def task: Task[Unit] = jfxTask2sfxTask(() => Platform.runLater({
    unit.compute()
    drawView()
    computed = false
  }))

  gc.font = Font.font(20)
  drawView()

  def operate(e: MouseEvent): Unit = {
    val se = jfxMouseEvent2sfx(e)
    val p = Point(se.x, se.y) map (n => Math.floor(n / blockSize) toInt)
    if (unit.view.legal.valid(BitBoard(p)) && unit.view.turn == Turn.Black) {
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

    def drawStone(color: Color, opacity: Double = 1)(pos: Point[Int]): Unit = {
      val margin = canvas.getWidth / 40
      val p = pos.map(_ * blockSize) + Point(margin, margin) * 0.5
      val r = blockSize - margin
      gc.fill = PaintIncludes.jfxColor2sfx(color opacity opacity)
      gc.fillOval(p.x, p.y, r, r)
    }

    gc.fill = Color.Green
    gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
    strokeLines(8, 8, blockSize)
    val black = BitBoard.toPoint(unit.view.black)
    val white = BitBoard.toPoint(unit.view.white)
    black foreach drawStone(Color.Black)
    white foreach drawStone(Color.White)

    gc.fill = Color.Gray
    gc.fillRect(100, 425, 200, 50)
    gc.fill = Color.White
    gc.fillOval(125, 435, 30, 30)
    gc.fill = Color.Black
    gc.fillOval(245, 435, 30, 30)
    gc.fillText(white.length.toString + " - " + black.length.toString, 170, 457.5)
    gc.fillRect(if (unit.view.turn == Turn.White) 125 else 245, 940, 60, 10)

    val legal = BitBoard.toPoint(unit.view.legal)
    // legal foreach drawStone(unit.view.turn != Turn.White, 0.5)
    if (black.length + white.length != 64 && legal.isEmpty) {
      unit.pass()
      drawView()
    } else if (unit.view.turn == Turn.White && !computed) {
      val t = new Thread(task)
      t.setDaemon(true)
      t.start()
      computed = true
    }
  }
}
