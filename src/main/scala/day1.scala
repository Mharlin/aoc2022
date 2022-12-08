@main def hello: Unit =
  println("Hello world!")
  println(day1(""))

def day1(calories: String): Int = {
  caloriesByElf(calories).max
}

def day1Part2(calories: String): Int = {
  caloriesByElf(calories).sorted.reverse.take(3).sum
}

private def caloriesByElf(calories: String): Seq[Int] = {
  calories
    .split(s"${System.lineSeparator()}${System.lineSeparator()}")
    .map(cals => cals.split(System.lineSeparator()).map(_.toInt).sum)
}
