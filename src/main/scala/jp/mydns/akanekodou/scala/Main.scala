package jp.mydns.akanekodou.scala

import math.Rational

// おまじない?
import math.RationalImplicits._
import scala.math.Fractional.Implicits._
import scala.math.Ordering.Implicits._

object Main extends App {
  val a = Rational(2, 12)
  val b = a.copy(n = 3)
  val c = a + b
  println(a, b, c, c.inverse, a == b, a < b)

  val d = Rational(1, 6)
  println(a == d, a.equals(d))

  val e = Rational(4, -8)
  println(e)
}
