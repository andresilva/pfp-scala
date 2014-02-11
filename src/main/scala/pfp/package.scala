package object pfp {
  import spire.math._

  type Spread[A, P] = Seq[A] => Distribution[A, P]
  type Transition[A, P] = A => Distribution[A, P]
  type Event[A] = A => Boolean
}
