package fhj.swengb.assignments.ttt.wsteinkellner

/**
  * Implement here your TicTacToe JavaFX App.
  */
object TicTacToeApp
{
  def main (args: Array[String])=
  {
    val init:Map[TMove, Player] = Map(TopLeft -> PlayerA, TopRight -> PlayerB, MiddleRight -> PlayerA)

    val ttt = new TicTacToe(init,PlayerB)
    println(ttt.asString())
    println(ttt.remainingMoves)
  }
}
