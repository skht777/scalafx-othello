

/**
  *
  * @author skht777
  *
  */
sealed trait Player

sealed case class Score(score: Int, put: Point[Int])

sealed case class AI() extends Player {
  private val MinScore = -10000
  private val MaxScore = 10000

  def calcLineScore(board: BitBoard): Int = {
    def score(pos: BitBoard, ds: Seq[Direction]): Int = {
      val shift = Function.chain(ds.map(d => (b: BitBoard) => d.legalShift(b, applyMask = false)))
      (1 to 2).foldLeft(Seq(pos))((s, _) => s :+ shift(s.last)).map(board.valid) match {
        case Seq(true, true, true) => 100
        case Seq(true, true, false) => 100
        case Seq(true, false, false) => 100
        case Seq(true, false, true) => 100
        case Seq(false, false, true) => 10
        case Seq(false, false, false) => 0
        case Seq(false, true, true) => -50
        case Seq(false, true, false) => -50
      }
    }

    val corner = Seq(0, 7)
    val corner4 = for (x <- corner; y <- corner) yield Point(x, y)

    corner4.map(p => {
      val (dir1, dir2) = (
        if (p.x == 0) Direction.Right else Direction.Left,
        if (p.y == 0) Direction.Down else Direction.Up
      )
      val dirSeq = for (d1 <- Seq(Some(dir1), None); d2 <- Seq(Some(dir2), None)) yield Seq(d1, d2).flatten

      dirSeq.map(score(BitBoard(p), _)).sum
    }).sum
  }

  def evaluate(current: BitBoard, opponent: BitBoard): Int = {
    val movablesScore = 100 * (current.length - opponent.length)
    val lineScore = 3 * (calcLineScore(current) - calcLineScore(opponent))

    movablesScore + lineScore
  }

  def calculate(state: State): Seq[Score] = {
    //@tailrec
    def alphaBetaEval(head: State, depth: Int, a: Int, b: Int): Int = {
      if (depth <= 0) return evaluate(head.current, head.opponent)
      val movables = BitBoard.toPoint(head.view.legal)
      if (movables.isEmpty) return -alphaBetaEval(State.pass(head), depth - 1, -b, -a)
      var _a = a
      movables.foreach(p => {
        val next = State.reverse(p)(head)
        _a = Math.max(_a, -alphaBetaEval(next, depth - 1, -b, -_a))
        if (_a >= b) return _a
      })

      _a
    }

    val movables = BitBoard.toPoint(state.view.legal)
    val limit = System.currentTimeMillis() + 500
    var scores: Seq[Score] = Seq()
    Iterator.from(3).takeWhile(_ => System.currentTimeMillis() < limit).foreach(depth => {
      scores = movables.map(p => Score(-alphaBetaEval(State.reverse(p)(state),
        depth - 1,
        -MaxScore,
        -MinScore
      ), p))
    })

    scores.sortBy(-_.score)
  }
}