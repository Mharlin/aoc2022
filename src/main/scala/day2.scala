enum Shape(val score: Int):
  case Rock extends Shape(1)
  case Paper extends Shape(2) 
  case Scissors extends Shape(3)

def day2(rounds: String) = {
  calculateGameScore(rounds)
}

def day2SecondPart(rounds: String) = {
  rounds.split(System.lineSeparator())
  .map(_.split(" ")).map {case Array(move, action) => (getMove(move), action)}.map {
    case (theirMove, "X") =>
      val myMove = getLoosingMove(theirMove)
      myMove.score + calculateRoundScore(theirMove, getLoosingMove(theirMove))
    case (theirMove, "Y") => theirMove.score + calculateRoundScore(theirMove, theirMove)
    case (theirMove, "Z") =>
      val myMove = getLoosingMove(getLoosingMove(theirMove))
      myMove.score + calculateRoundScore(theirMove, myMove)
  }.sum
}

def getLoosingMove(move: Shape): Shape = move match {
  case Shape.Rock => Shape.Scissors
  case Shape.Paper => Shape.Rock
  case Shape.Scissors => Shape.Paper
}

def calculateGameScore(rounds: String): Int = {
  rounds
    .split(System.lineSeparator())
    .map(_.split(" ").map(getMove))
    .map { case Array(thierMove, myMove) => myMove.score + calculateRoundScore(thierMove, myMove) }.sum
}

def getMove(move: String): Shape = move match {
      case "A" | "X" => Shape.Rock
      case "B" | "Y" => Shape.Paper
      case "C" | "Z" => Shape.Scissors
    }

def calculateRoundScore(thirMove: Shape, myMove: Shape): Int = (thirMove, myMove) match {
  case (t, m) if t == m => 3
  case (Shape.Scissors, Shape.Rock) => 6
  case (Shape.Rock, Shape.Paper) => 6
  case (Shape.Paper, Shape.Scissors) => 6
  case _ => 0
}
