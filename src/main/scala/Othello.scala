import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.concurrent.Task
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.{Color, PaintIncludes}
import scalafx.scene.text.{Font, Text}
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
  private[this] val r = 30
  private[this] val x = canvas.getWidth / 4
  private[this] val y = canvas.getWidth
  private[this] val h = (canvas.getHeight - y) / 2
  private[this] val margin = x / 4
  private[this] val whiteStone = x + margin
  private[this] val blackStone = x * 3 - r - margin
  private[this] val stoneHeight = y + h - r / 2
  private[this] val score = "%-2d - %-2d"
  gc.font = Font.font(24)
  drawView()

  def operate(e: MouseEvent): Unit = {
    val se = jfxMouseEvent2sfx(e)
    val p = Point(se.x, se.y) map (n => Math.floor(n / blockSize) toInt)
    if (unit.view.legal.valid(BitBoard(p)) && unit.view.turn == Turn.Black) {
      unit.reverse(p)
      drawView()
    }
  }

  private[this] def task: Task[Unit] = jfxTask2sfxTask(() => Platform.runLater({
    unit.compute()
    drawView()
    computed = false
  }))

  private[this] def getTextSize(target: String, font: Font): Point[Double] = {
    val text = new Text(target)
    text.font = font
    Point(text.getLayoutBounds.getWidth, text.getLayoutBounds.getHeight)
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
    val text = score.format(white.length, black.length)
    val textSize = getTextSize(text, gc.font) * 0.5
    val turn = if (unit.view.turn == Turn.White) whiteStone else blackStone
    black foreach drawStone(Color.Black)
    white foreach drawStone(Color.White)
    gc.fill = Color.Gray
    gc.fillRect(x, y + h / 2, x * 2, h)
    gc.fill = Color.White
    gc.fillOval(whiteStone, stoneHeight, r, r)
    gc.fill = Color.Black
    gc.fillOval(blackStone, stoneHeight, r, r)
    gc.fillText(text, x * 2 - textSize.x, y + h + textSize.y / 2)
    gc.fillRect(turn, y + h + h / 2 - 5, r, 5)

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
