package pfp

abstract class Suite[H, D](var d: Distribution[H, Double]) {
  def likelihood(data: D, hypo: H): Double
  def update(data: D) { d = d.scale { case (h, prob) => prob * likelihood(data, h) } }

  def plot() = d.plot
}

class Dice(hypos: Seq[Int]) extends Suite[Int, Int](Distribution.uniform[Int, Double].apply(hypos)) {
  def likelihood(data: Int, hypo: Int) =
    if(hypo < data) 0 else 1.0 / hypo
}

object ExampleDice extends App {
  val suite = new Dice(List(4, 6, 8, 12, 20))

  println("Priors:")
  suite.plot()

  println()
  println("After a 6 is rolled:")
  suite.update(6)
  suite.plot()

  println()
  println("After 6, 8, 7, 7, 5, 4 are rolled after the first 6:")
  List(6, 8, 7, 7, 5, 4).foreach(suite.update)
  suite.plot()
}