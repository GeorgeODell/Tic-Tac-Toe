package mainPackage

class gameBoard {
  // constructor is the class body
  var boardArray = Array.ofDim[Char](3, 3)
  boardArray = Array(Array(' ', ' ', ' '), Array(' ', ' ', ' '), Array(' ', ' ', ' '))
  var boardString = new String
  updateBoard

  def updateBoard: Unit = {
    boardString =
      s"""
         |   |   |
         | ${boardArray(0)(0)} | ${boardArray(0)(1)} | ${boardArray(0)(2)}
         |   |   |
         |-----------
         |   |   |
         | ${boardArray(1)(0)} | ${boardArray(1)(1)} | ${boardArray(1)(2)}
         |   |   |
         |-----------
         |   |   |
         | ${boardArray(2)(0)} | ${boardArray(2)(1)} | ${boardArray(2)(2)}
         |   |   |
       """.stripMargin
  }

  def showBoard: Unit = {
    println(s"$boardString\n")
  }

  def isPositionEmpty(position: Char): Boolean = position match {
    case 'X' => println("Invalid move."); false
    case 'O' => println("Invalid move."); false
    case _ => true
  }

  def doMove(playerType: Char, playerMove: String): Unit = {
    playerMove match {
      case "TL" => if (isPositionEmpty(boardArray(0)(0))) {boardArray(0)(0) = playerType}
      case "TM" => if (isPositionEmpty(boardArray(0)(1))) {boardArray(0)(1) = playerType}
      case "TR" => if (isPositionEmpty(boardArray(0)(2))) {boardArray(0)(2) = playerType}
      case "ML" => if (isPositionEmpty(boardArray(1)(0))) {boardArray(1)(0) = playerType}
      case "MM" => if (isPositionEmpty(boardArray(1)(1))) {boardArray(1)(1) = playerType}
      case "MR" => if (isPositionEmpty(boardArray(1)(2))) {boardArray(1)(2) = playerType}
      case "BL" => if (isPositionEmpty(boardArray(2)(0))) {boardArray(2)(0) = playerType}
      case "BM" => if (isPositionEmpty(boardArray(2)(1))) {boardArray(2)(1) = playerType}
      case "BR" => if (isPositionEmpty(boardArray(2)(2))) {boardArray(2)(2) = playerType}
      case _ => println("Invalid move.")
    }
  }

  def checkForEnd: Char = {
    if (boardArray(0)(0) == boardArray(0)(1) && boardArray(0)(0) == boardArray(0)(2)) {
      return boardArray(0)(0)
    } else if (boardArray(1)(0) == boardArray(1)(1) && boardArray(1)(0) == boardArray(1)(2)) {
      return boardArray(1)(0)
    } else if (boardArray(2)(0) == boardArray(2)(1) && boardArray(2)(0) == boardArray(2)(2)) {
      return boardArray(2)(0)
    } else if (boardArray(0)(0) == boardArray(1)(0) && boardArray(0)(0) == boardArray(2)(0)) {
      return boardArray(0)(0)
    } else if (boardArray(0)(1) == boardArray(1)(1) && boardArray(0)(1) == boardArray(2)(1)) {
      return boardArray(0)(1)
    } else if (boardArray(0)(2) == boardArray(1)(2) && boardArray(0)(2) == boardArray(2)(2)) {
      return boardArray(0)(2)
    } else if (boardArray(0)(0) == boardArray(1)(1) && boardArray(0)(0) == boardArray(2)(2)) {
      return boardArray(0)(0)
    } else if (boardArray(0)(2) == boardArray(1)(1) && boardArray(0)(2) == boardArray(2)(0)) {
      return boardArray(0)(2)
    } else if (boardArray.filterNot(_.filter(_ == ' ').isEmpty).isEmpty) {
      return 'T'
    } else {
      return ' '
    }
  }
}

object mainObject extends App {
  var currentGameBoard = new gameBoard
  var playerInput = new String
  var playerType = 'X'
  var status = ' '

  currentGameBoard.showBoard

  while (status == ' ') {
    playerInput = scala.io.StdIn.readLine(s"$playerType enter your move: ")
    currentGameBoard.doMove(playerType, playerInput)
    currentGameBoard.updateBoard
    currentGameBoard.showBoard
    status = currentGameBoard.checkForEnd

    // checks if the game has ended
    if (status != ' ') {
      if (status == 'T') {
        println("Draw.")
      } else {
        println(s"$status wins!")
      }
    }

    // alternating players
    if (playerType == 'X') {
      playerType = 'O'
    } else {
      playerType = 'X'
    }
  }
}
