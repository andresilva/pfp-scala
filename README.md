pfp-scala
=========

Probabilistic Functional Programming in Scala.

Here's an example modeling the [Monty Hall problem](http://en.wikipedia.org/wiki/Monty_Hall_problem):

```scala
sealed trait Outcome
case object Win extends Outcome
case object Lose extends Outcome

def firstChoice = uniform[Outcome, Rational].apply(Win :: Lose :: Lose :: Nil)

def switch(o: Outcome) = o match {
  case Win => certainly[Outcome, Rational](Lose)
  case Lose => certainly[Outcome, Rational](Win)
}

// Let's test both strategies

firstChoice.plot()
// Win 1/3 #################################
// Lose 2/3 ##################################################################

firstChoice.flatMap(switch).plot()
// Win 2/3 ##################################################################
// Lose 1/3 #################################
```

Based on *Probabilistic Functional Programming in Haskell, Martin Erwig and Steve Kollmansberger 
Journal of Functional Programming, Vol. 16, No. 1, 21-34, 2006* [[1]]

[1]: http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
