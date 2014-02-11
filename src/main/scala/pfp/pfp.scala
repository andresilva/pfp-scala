package pfp

import scalaz.{ Kleisli, Monad, MonadPlus }
import scalaz.Kleisli.kleisli
import scalaz.syntax.monadPlus._
import spire.algebra.Trig
import spire.compat._
import spire.math._
import spire.implicits._

case class Distribution[A, P: Numeric](data: Stream[(A, P)]) extends Function[Event[A], P] {
  def apply(event: Event[A]) = data.filter(d => event(d._1)).map(_._2).sum

  def normalized = data.groupBy(_._1).mapValues(_.map(_._2).sum).toStream

  def plot(implicit ord: Ordering[A] = null) =
    if (data.isEmpty) println("impossible")
    else {
      val data = if (ord == null) normalized else normalized.sortBy(_._1)
      val scale = Numeric[P].fromInt(100)
      val maxWidth = data.map(_._1.toString.length).max
      val fmt = "%" + maxWidth + "s %s %s"
      data.foreach { case (b, p) =>
        val hashes = (p * scale).toInt
        println(fmt.format(b.toString, p.toString, "#" * hashes))
      }
    }
}

object Distribution {
  implicit def distributionMonad[P: Numeric] = new MonadPlus[({type λ[α] = Distribution[α, P]})#λ] {
    def plus[A](a: Distribution[A, P], b: => Distribution[A, P]): Distribution[A,P] = ???

    def empty[A]: Distribution[A, P] = Distribution(Stream.empty[(A, P)])

    def point[A](a: => A) = certainly[A, P](a)
    def bind[A, B](fa: Distribution[A, P])(f: A => Distribution[B, P]) =
      Distribution {
        for {
          (a, p) <- fa.data
          (b, q) <- f(a).data
        } yield ((b, p * q))
      }
  }

  def certainly[A, P: Numeric](a: A) = Distribution[A, P](Stream(a -> Numeric[P].one))

  def impossible[A, P: Numeric] = MonadPlus[({type λ[α] = Distribution[α, P]})#λ].empty[A]

  def choose[A, P: Numeric](a: A, b: A)(p: P) =
    Distribution(Stream(a -> p, b -> (1 - p)))

  def enum[A, P: Fractional](is: Int*): Spread[A, P] =
    as => fromFreqs(as.zip(is.map(Fractional[P].fromInt(_))): _*)

  def relative[A, P: Fractional](ns: P*): Spread[A, P] =
    as => fromFreqs(as.zip(ns): _*)

  def shape[A, P: Fractional](f: P => P): Spread[A, P] = {
    case Nil => throw new IllegalArgumentException("empty list")
    case as =>
      val incr =
        if (as.length - 1 == 0) Fractional[P].zero
        else Fractional[P].one / (as.length - 1)
      def ps = Stream.iterate(Fractional[P].zero)(_ + incr).map(f)
      fromFreqs(as.zip(ps): _*)
  }

  def linear[A, P: Fractional]: Spread[A, P] = shape(Shape.linear)
  def uniform[A, P: Fractional]: Spread[A, P] = shape(Shape.uniform)
  def negExp[A, P: Fractional: Trig]: Spread[A, P] = shape(Shape.negExp)
  def normal[A, P: Fractional: Trig]: Spread[A, P] = shape(Shape.normal)

  def tf[P: Numeric](p: P) = choose(true, false)(p)
  def bernoulli[P: Numeric](p: P) = choose(1, 0)(p)


  def joinWith[A, B, C, P: Numeric](f: (A, B) => C)(
    a: Distribution[A, P], b: Distribution[B, P]): Distribution[C, P] =
    Distribution {
      for {
        (a, p) <- a.data
        (b, q) <- b.data
      } yield (f(a, b), p * q)
    }
  def prod[A, B, P: Numeric](a: Distribution[A, P], b: Distribution[B, P]) =
    joinWith[A, B, (A, B), P]((a, b) => (a, b))(a, b)

  def die[P: Fractional] = uniform[Int, P].apply(1 to 6)

  def dice[P: Fractional](n: Int): Distribution[List[Int], P] = n match {
    case 0 => certainly[List[Int], P](Nil)(util.frac2num)
    case n => joinWith[Int, List[Int], List[Int], P](_ :: _)(die, dice(n -1))(util.frac2num)
  }

  def compose[A, M[+_]: Monad](aps: Seq[A => M[A]]): A => M[A] =
    aps.map(kleisli[M, A, A]).foldLeft(Kleisli.ask[M, A])((a, b) => a >=> b)

  def selectOne[A, P: Fractional](as: A*): Distribution[(A, Seq[A]), P] =
    uniform[(A, Seq[A]), P].apply(as.map(a => (a, as.diff(List(a)))))

  def selectMany[A, P: Fractional: Numeric](n: Int, as: A*): Distribution[(Seq[A], Seq[A]), P] = n match {
    case 0 => Monad[({type λ[α] = Distribution[α, P]})#λ].point((Nil, as))
    case n => for {
      (x, c1) <- selectOne[A, P](as: _*)
      (xs, c2) <- selectMany[A, P](n - 1, c1: _*)
    } yield (x +: xs, c2)
  }

  def select[A, P: Fractional: Numeric](n: Int, as: A*): Distribution[Seq[A], P] =
    selectMany[A, P](n, as: _*).map(_._1.reverse)

  private[this] def fromFreqs[A, P: Fractional](data: (A, P)*): Distribution[A, P] = {
    val q = data.map(_._2).sum
    Distribution(data.map { case (a, p) => (a, p / q) }.toStream)(util.frac2num)
  }
}

object Shape {
  type Shape[P] = P => P

  def linear[P: Fractional]: Shape[P] = identity
  def uniform[P: Fractional]: Shape[P] = _ => Fractional[P].one
  def negExp[P: Fractional: Trig]: Shape[P] = p => Trig[P].exp(-p)
  def normal[P: Fractional: Trig]: Shape[P] =
    normalCurve(Fractional[P].fromDouble(0.5), Fractional[P].fromDouble(0.5))

  private[this] def normalCurve[P: Fractional: Trig](mean: P, dev: P)(p: P) = {
    val u = (p - mean) / dev
    Trig[P].exp((-Fractional[P].one) / 2 * (u ** 2)) / (Trig[P].pi * 2).sqrt
  }
}
