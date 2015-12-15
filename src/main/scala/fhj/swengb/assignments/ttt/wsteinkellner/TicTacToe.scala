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
    if(winner!=None || moveHistory.size ==9 )
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
  //lazy val nextGames: Set[TicTacToe] = ???

  /**
    * Either there is no winner, or PlayerA or PlayerB won the game.
    *
    * The set of moves contains all moves which contributed to the result.
    */
  def winner: Option[(Player, Set[TMove])] =
  {
    if(moveHistory.size >= 6)
      {
        if(moveHistory.contains(TopLeft) && moveHistory.contains(TopCenter) && moveHistory.contains(TopRight))
          {
            if(moveHistory(TopLeft) == moveHistory(TopCenter) == moveHistory(TopCenter) == moveHistory(TopRight))
            {
              val set: Set[TMove] = Set(TopLeft, TopCenter, TopRight)
              return Some(moveHistory(TopLeft), set)
            }
            else return None
          }
        else if(moveHistory.contains(MiddleLeft) && moveHistory.contains(MiddleCenter) && moveHistory.contains(MiddleRight))
        {
          if(moveHistory(MiddleLeft) == moveHistory(MiddleCenter) == moveHistory(TopRight))
          {
            val set: Set[TMove] = Set(MiddleLeft, MiddleCenter, MiddleRight)
            return Some(moveHistory(MiddleLeft), set)
          }
          else return None
        }

        else if(moveHistory.contains(BottomLeft) && moveHistory.contains(BottomCenter) && moveHistory.contains(BottomRight))
        {
          if(moveHistory(BottomLeft) == moveHistory(BottomCenter) == moveHistory(BottomRight))
          {
            val set: Set[TMove] = Set(BottomLeft, BottomCenter, BottomRight)
            return Some(moveHistory(BottomLeft), set)
          }
          else return None
        }

        else if(moveHistory.contains(TopLeft) && moveHistory.contains(MiddleCenter) && moveHistory.contains(BottomRight))
        {
          if(moveHistory(TopLeft) == moveHistory(MiddleCenter) == moveHistory(BottomRight))
          {
            val set: Set[TMove] = Set(TopLeft, MiddleCenter, BottomRight)
            return Some(moveHistory(TopLeft), set)
          }
          else return None
        }
        else if(moveHistory.contains(TopRight) && moveHistory.contains(MiddleCenter) && moveHistory.contains(BottomLeft))
        {
          if(moveHistory(TopRight) == moveHistory(MiddleCenter) == moveHistory(BottomLeft))
          {
            val set: Set[TMove] = Set(TopRight, MiddleCenter, BottomLeft)
            return Some(moveHistory(TopRight), set)
          }
          else return None
        }

        else if(moveHistory.contains(TopLeft) && moveHistory.contains(MiddleLeft) && moveHistory.contains(BottomLeft))
        {
          if(moveHistory(TopLeft) == moveHistory(MiddleLeft) == moveHistory(BottomLeft))
          {
            val set: Set[TMove] = Set(TopLeft, MiddleLeft, BottomLeft)
            return Some(moveHistory(TopLeft), set)
          }
          else return None
        }

        else if(moveHistory.contains(TopCenter) && moveHistory.contains(MiddleCenter) && moveHistory.contains(BottomCenter))
        {
          if(moveHistory(TopCenter) == moveHistory(MiddleCenter) == moveHistory(BottomCenter))
          {
            val set: Set[TMove] = Set(TopCenter, MiddleCenter, BottomCenter)
            return Some(moveHistory(TopCenter), set)
          }
          else return None
        }

        else if(moveHistory.contains(TopRight) && moveHistory.contains(MiddleRight) && moveHistory.contains(BottomRight))
        {
          if(moveHistory(TopRight) == moveHistory(MiddleRight) == moveHistory(BottomRight))
          {
            val set: Set[TMove] = Set(TopRight, MiddleRight, BottomRight)
            return Some(moveHistory(TopRight), set)
          }
          else return None
        }


        else return None
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
  //def turn(p: TMove, player: Player): TicTacToe = ???

}


