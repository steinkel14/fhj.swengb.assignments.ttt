package fhj.swengb.assignments.ttt.wsteinkellner

import scala.collection.Set

/**
  * models the different moves the game allows
  *
  * each move is made by either player a or player b.
  */
sealed trait TMove {
  def idx: Int
}

case object TopLeft extends TMove {
  override def idx: Int = 0
}

case object TopCenter extends TMove {
  override def idx: Int = 1
}

case object TopRight extends TMove {
  override def idx: Int = 2
}

case object MiddleLeft extends TMove {
  override def idx: Int = 3
}

case object MiddleCenter extends TMove {
  override def idx: Int = 4
}

case object MiddleRight extends TMove {
  override def idx: Int = 5
}

case object BottomLeft extends TMove {
  override def idx: Int = 6
}

case object BottomCenter extends TMove {
  override def idx: Int = 7
}

case object BottomRight extends TMove {
  override def idx: Int = 8
}


/**
  * for a tic tac toe game, there are two players, player A and player B
  */
sealed trait Player

case object PlayerA extends Player

case object PlayerB extends Player

object TicTacToe {

  /**
    * creates an empty tic tac toe game
    * @return
    */
  def apply(): TicTacToe = TicTacToe(null)

  /**
    * For a given tic tac toe game, this function applies all moves to the game.
    * The first element of the sequence is also the first move.
    *
    * @param t
    * @param moves
    * @return
    */
  //def play(t: TicTacToe, moves: Seq[TMove]): TicTacToe = ???

  /**
    * creates all possible games.
    * @return
    */
  //def mkGames(): Map[Seq[TMove], TicTacToe] = ???

}

/**
  * Models the well known tic tac toe game.
  *
  * The map holds the information which player controls which field.
  *
  * The nextplayer parameter defines which player makes the next move.
  */
case class TicTacToe(moveHistory: Map[TMove, Player],
                     nextPlayer: Player = PlayerA) {

  /**
    * outputs a representation of the tic tac toe like this:
    *
    * |---|---|---|
    * | x | o | x |
    * |---|---|---|
    * | o | x | x |
    * |---|---|---|
    * | x | o | o |
    * |---|---|---|
    *
    *
    * @return
    */
  def asString(): String =
  {
    val temp = Array(" "," "," "," "," "," "," "," "," ")

    for((key, value) <- moveHistory)
      {
        if (value == PlayerA)
          {
            temp(key.idx) = "X"
          }
        else
          {
            temp(key.idx) = "O"
          }
      }

    val field : String = "|---|---|---|\n" +
      "| " + temp(0) + " | " + temp(1) + " | " + temp(2) + " |\n" +
      "|---|---|---|\n" +
      "| " + temp(3) + " | " + temp(4) + " | " + temp(5) + " |\n" +
      "|---|---|---|\n" +
      "| " + temp(6) + " | " + temp(7) + " | " + temp(8) + " |\n" +
      "|---|---|---|"

    field
  }

  /**
    * is true if the game is over.
    *
    * The game is over if either of a player wins or there is a draw.
    */
  val gameOver : Boolean =
  {
    if(winner == Some(PlayerA) || winner == Some(PlayerB) || moveHistory.size ==9 )
      {
        true
      }
    else false
  }

  /**
    * the moves which are still to be played on this tic tac toe.
    */
  val remainingMoves: Set[TMove] =
  {
    var remain:Set[TMove] = Set(BottomLeft,BottomCenter,BottomRight,MiddleLeft,MiddleCenter,MiddleRight,TopLeft,TopCenter,TopRight)
    for((key, value) <- moveHistory)
    {
      remain -= key
    }
    remain
  }

  /**
    * given a tic tac toe game, this function returns all
    * games which can be derived by making the next turn. that means one of the
    * possible turns is taken and added to the set.
    */
  lazy val nextGames: Set[TicTacToe] =
  {
    var ttt: Set[TicTacToe] = Set()
    var nextP = nextPlayer
    if(nextPlayer == PlayerA)
      {
        nextP = PlayerB
      }
    else nextP = PlayerA

    var freeField:Set[TMove] = Set(TopLeft,TopCenter,TopRight,MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)
    for((key,value)<- moveHistory){
      freeField -= key
    }
    for(el <- freeField){
      val addTicTacToe = Set(TicTacToe(Map(el->nextPlayer),nextP ))
      ttt = ttt ++ addTicTacToe
    }
    ttt
  }

  /**
    * Either there is no winner, or PlayerA or PlayerB won the game.
    *
    * The set of moves contains all moves which contributed to the result.
    */
  def winner: Option[(Player, Set[TMove])] = {
    if ((moveHistory(TopLeft) == moveHistory(TopCenter) == moveHistory(TopRight)) && moveHistory(TopLeft) != null) {
      val set: Set[TMove] = Set(TopLeft, TopCenter, TopRight)
      Some(moveHistory(TopLeft), set)
    }

    else if ((moveHistory(MiddleLeft) == moveHistory(MiddleCenter) == moveHistory(MiddleRight)) && moveHistory(MiddleLeft) != null) {
      val set: Set[TMove] = Set(MiddleLeft, MiddleCenter, MiddleRight)
      Some(moveHistory(MiddleRight), set)
    }

    else if ((moveHistory(BottomLeft) == moveHistory(BottomCenter) == moveHistory(BottomRight)) && moveHistory(BottomLeft) != null) {
      val set: Set[TMove] = Set(BottomLeft, BottomCenter, BottomRight)
      Some(moveHistory(BottomLeft), set)
    }

    else if ((moveHistory(TopLeft) == moveHistory(MiddleLeft) == moveHistory(BottomLeft)) && moveHistory(TopLeft) != null) {
      val set: Set[TMove] = Set(TopLeft, MiddleLeft, BottomLeft)
      Some(moveHistory(TopLeft), set)
    }

    else if ((moveHistory(TopCenter) == moveHistory(MiddleCenter) == moveHistory(BottomCenter)) && moveHistory(TopCenter) != null) {
      val set: Set[TMove] = Set(TopCenter, MiddleCenter, BottomCenter)
      Some(moveHistory(TopCenter), set)
    }

    else if ((moveHistory(TopRight) == moveHistory(MiddleRight) == moveHistory(BottomRight)) && moveHistory(TopRight) != null) {
      val set: Set[TMove] = Set(TopRight, MiddleRight, BottomRight)
      Some(moveHistory(TopRight), set)
    }

    else if ((moveHistory(TopLeft) == moveHistory(MiddleCenter) == moveHistory(BottomRight)) && moveHistory(TopLeft) != null) {
      val set: Set[TMove] = Set(TopLeft, MiddleCenter, BottomRight)
      Some(moveHistory(TopLeft), set)
    }

    else if ((moveHistory(TopRight) == moveHistory(MiddleCenter) == moveHistory(BottomLeft)) && moveHistory(TopRight) != null) {
      val set: Set[TMove] = Set(TopRight, MiddleCenter, BottomLeft)
      Some(moveHistory(TopRight), set)
    }

    else None
  }

  /**
    * returns a copy of the current game, but with the move applied to the tic tac toe game.
    *
    * @param move to be played
    * @param player the player
    * @return
    */
  def turn(p: TMove, player: Player): TicTacToe = {
    val map = Map(p -> player)
    println("Turn" + moveHistory)
    val addedMove = moveHistory ++ map
    println("Turn" + addedMove)
    if (player == PlayerA) {
      TicTacToe(addedMove, PlayerB)
    } else {
      TicTacToe(addedMove, PlayerA)
    }
  }
}


