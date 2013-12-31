package pfp

import spire.algebra.Trig
import spire.math._

object util {
  def frac2num[A: Fractional]: Numeric[A] = new Numeric[A] {
    def negate(x: A): A = Fractional[A].negate(x)
    def zero: A = Fractional[A].zero
    def plus(x: A, y: A): A = Fractional[A].plus(x, y)

    def toBigDecimal(a: A): BigDecimal = Fractional[A].toBigDecimal(a)
    def toBigInt(a: A): BigInt = Fractional[A].toBigInt(a)
    def toByte(a: A): Byte = Fractional[A].toByte(a)
    def toFloat(a: A): Float = Fractional[A].toFloat(a)
    def toInt(a: A): Int = Fractional[A].toInt(a)
    def toLong(a: A): Long = Fractional[A].toLong(a)
    def toDouble(a: A): Double = Fractional[A].toDouble(a)
    def toNumber(a: A): Number = Fractional[A].toNumber(a)
    def toRational(a: A): Rational = Fractional[A].toRational(a)
    def toShort(a: A): Short = Fractional[A].toShort(a)
    def toString(a: A): String = Fractional[A].toString(a)
    def toType[B](a: A)(implicit ev: ConvertableTo[B]): B = ev.fromType(a)

    def fromBigDecimal(n: BigDecimal): A = Fractional[A].fromBigDecimal(n)
    def fromBigInt(n: BigInt): A = Fractional[A].fromBigInt(n)
    def fromByte(n: Byte): A = Fractional[A].fromByte(n)
    def fromDouble(n: Double): A = Fractional[A].fromDouble(n)
    def fromFloat(n: Float): A = Fractional[A].fromFloat(n)
    def fromLong(n: Long): A = Fractional[A].fromLong(n)
    def fromRational(n: Rational): A = Fractional[A].fromRational(n)
    def fromShort(n: Short): A = Fractional[A].fromShort(n)
    def fromType[B](b: B)(implicit ev: ConvertableFrom[B]): A = ev.toType(b)

    def ceil(a: A): A = Fractional[A].ceil(a)
    def floor(a: A): A = Fractional[A].floor(a)
    def isWhole(a: A): Boolean = Fractional[A].isWhole(a)
    def round(a: A): A = Fractional[A].round(a)

    def div(x: A, y: A): A = Fractional[A].div(x, y)

    def one: A = Fractional[A].one

    def times(x: A, y: A): A = Fractional[A].times(x, y)

    def fpow(a: A, b: A): A = Fractional[A].fpow(a, b)
    def nroot(a: A, n: Int): A = Fractional[A].nroot(a, n)

    def compare(x: A, y: A): Int = Fractional[A].compare(x, y)

    def abs(a: A): A = Fractional[A].abs(a)
    def signum(a: A): Int = Fractional[A].signum(a)
  }
}
