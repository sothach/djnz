package temp

import scala.math.Ordered

trait Temperature extends Ordered[Temperature] {
  def kelvin: Double
  protected val kfOffset = 32
  protected val kcOffset = 273.15
  protected val krFactor = 9.0 / 5.0
  protected[temp] def toC = Celsius(kelvin - kcOffset)
  protected[temp] def toF = Fahrenheit((kelvin - kcOffset) * krFactor + kfOffset)
  protected[temp] def toR = Rankine(kelvin * krFactor)
  override def compare(other: Temperature) = round(kelvin).compare(round(other.kelvin))
  override def equals(o: Any) = o match {
    case t: Temperature => compare(t) == 0
    case _ => false
  }
  override def hashCode = super.hashCode
  private def round(d: Double) = (d * 100).round / 100.0
}

case class Kelvin(kelvin: Double) extends Temperature {
  override def toString = f"$kelvin%4.2fK"
}

case class Celsius(celsius: Double) extends Temperature {
  val kelvin = celsius + kcOffset
  override def toString = f"$celsius%.2f\u00B0C"
}

case class Fahrenheit(fahrenheit: Double) extends Temperature {
  val kelvin = (fahrenheit - kfOffset) * (1 / krFactor) + kcOffset
  override def toString = f"$fahrenheit%.2f\u00B0F"
}

case class Rankine(rankine: Double) extends Temperature {
  val kelvin = rankine / krFactor
  override def toString = f"$rankine%.2f\u00B0Ra"
}

object Temperature {
  implicit def toCelsius(k: Kelvin): Celsius = k.toC
  implicit def toFahrenheit(k: Kelvin): Fahrenheit = k.toF
  implicit def toRankine(k: Kelvin): Rankine = k.toR
  implicit def toKelvin(c: Celsius): Kelvin = Kelvin(c.kelvin)
  implicit def toKelvin(f: Fahrenheit): Kelvin = Kelvin(f.kelvin)
  implicit def toKelvin(r: Rankine): Kelvin = Kelvin(r.kelvin)
  implicit def k2Double(k: Kelvin): Double = k.kelvin
  implicit def c2Double(c: Celsius): Double = c.celsius
  implicit def f2Double(f: Fahrenheit): Double = f.fahrenheit
  implicit def r2Double(r: Rankine): Double = r.rankine
  implicit def double2K(d: Double): Kelvin = Kelvin(d)
  implicit def double2C(d: Double): Celsius = Celsius(d)
  implicit def double2F(d: Double): Fahrenheit = Fahrenheit(d)
  implicit def double2R(d: Double): Rankine = Rankine(d)
}
