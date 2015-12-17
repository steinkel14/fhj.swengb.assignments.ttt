package fhj.swengb.assignments.ttt.wsteinkellner

import java.net.URL
import java.util.ResourceBundle
import javafx.fxml.{FXML, Initializable, FXMLLoader}
import javafx.scene.control.Button
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import scala.util.control.NonFatal

/**
  * Implement here your TicTacToe JavaFX App.
  */
object TicTacToeApp
{
  def main (args: Array[String])=
  {
    val init:Map[TMove, Player] = Map(TopLeft -> PlayerA, TopRight -> PlayerA, TopCenter -> PlayerA)

    val ttt = new TicTacToe(init,PlayerB)
    println(ttt.asString())
    println(ttt.remainingMoves)
  }
}

class TicTacToeApp extends javafx.application.Application
{
  val fxml = "/fhj/swengb/assignments/ttt/TicTacToeApp.fxml"
  val css = "/fhj/swengb/assignments/ttt/TicTacToeApp.css"

  def makeFxmlLoader(fxml: String): FXMLLoader =
  {
    new FXMLLoader(getClass.getResource(fxml))
  }

  override def start(stage: Stage): Unit =
  {
    try {
      stage.setTitle("TicTacToeGame")
      setSkin(stage, fxml, css)
      stage.show()
      stage.setMinWidth(1000)
      stage.setMinHeight(1000)
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
  }
    def setSkin(stage: Stage, fxml: String, css: String): Boolean =
    {
      val scene = new Scene(makeFxmlLoader(fxml).load[Parent]())
      stage.setScene(scene)
      stage.getScene.getStylesheets.clear()
      stage.getScene.getStylesheets.add(css)
    }
}

class TicTacToeController extends Initializable
{
  var currentPlayer: Player = PlayerA
  var moveHistory: Map[TMove, Player] = Map()
  var TicTacToeG = new TicTacToe(moveHistory, currentPlayer)

  override def initialize(location: URL, resources: ResourceBundle): Unit =
  {

  }

  @FXML var btnTL: Button = _
  @FXML var btnTC: Button = _
  @FXML var btnTR: Button = _
  @FXML var btnML: Button = _
  @FXML var btnMC: Button = _
  @FXML var btnMR: Button = _
  @FXML var btnBL: Button = _
  @FXML var btnBC: Button = _
  @FXML var btnBR: Button = _

  def setLabel(button: Button, move: TMove) =
  {
    if(TicTacToeG.nextPlayer == PlayerA)
      {
        button.setText("X")
        button.setDisable(true)
      }
    else
      {
        button.setText("O")
        button.setDisable(true)
      }
  }

  def btnClickTL(): Unit = setLabel(btnTL, TopLeft)
  def btnClickTC(): Unit = setLabel(btnTC, TopCenter)
  def btnClickTR(): Unit = setLabel(btnTR, TopRight)
  def btnClickML(): Unit = setLabel(btnML, MiddleLeft)
  def btnClickMC(): Unit = setLabel(btnMC, MiddleCenter)
  def btnClickMR(): Unit = setLabel(btnMR, MiddleRight)
  def btnClickBL(): Unit = setLabel(btnBL, BottomLeft)
  def btnClickBC(): Unit = setLabel(btnBC, BottomCenter)
  def btnClickBR(): Unit = setLabel(btnBR, BottomRight)
}
