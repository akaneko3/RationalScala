package jp.mydns.akanekodou.scala.math

// 分数クラス(分母はデフォルトで 1)
case class Rational(n : BigInt, d : BigInt = 1) {
  require(d != 0) // 分母は 0 でないことを要求する

  // あらかじめ約分
  private val g : BigInt = n gcd d
  val numer : BigInt = n * d.signum / g
  val denom : BigInt = d * d.signum / g

  // 逆数
  def inverse : Rational = new Rational(denom, numer)

  // toString の override
  override def toString() : String =
    numer + (if (denom == 1) ""  else ("/" + denom))

  // equals の override
  override def equals(other : Any) = {
    other match {
      case that: Rational =>
        that.canEqual(Rational.this) && (numer * that.denom == denom * that.numer)
      case _              => false
    }
  }
}

object RationalImplicits {
  implicit object RationalIsFractional extends Fractional[Rational] {
    // compare の実装
    def compare(x : Rational, y : Rational) : Int =
      (x.numer * y.denom) compare (x.denom * y.numer)

    // 四則演算
    def plus(x : Rational, y : Rational) : Rational =
      Rational(x.numer * y.denom + x.denom * y.numer, x.denom * y.denom)
    def minus(x : Rational, y : Rational) : Rational =
      Rational(x.numer * y.denom - x.denom * y.numer, x.denom * y.denom)
    def times(x : Rational, y : Rational) : Rational =
      Rational(x.numer * y.numer, x.denom * y.denom)
    def div(x : Rational, y : Rational) : Rational =
      Rational(x.numer * y.denom, x.denom * y.numer)

    // 正負反転
    def negate(x : Rational) : Rational = Rational(- x.numer, x.denom)

    // 型変換
    def fromInt(x : Int) : Rational = Rational(x)
    def toInt(x : Rational) : Int = (x.numer / x.denom).toInt
    def toLong(x : Rational) : Long = (x.numer / x.denom).toLong
    def toFloat(x : Rational) : Float =
      x.numer.toFloat / x.denom.toFloat
    def toDouble(x : Rational) : Double =
      x.numer.toDouble / x.denom.toDouble
  }
}
