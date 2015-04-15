import org.scalatest._

class TemperatureConversion extends FlatSpec with Matchers {
  import temp._

  "temperature list" should "be sorted correctly" in {
    val temps: List[Temperature] = List(Celsius(20),Fahrenheit(19.7),Rankine(199.0),Kelvin(2))
    val klist = temps.sorted map(v=>(v.kelvin * 100).round / 100.0)
    klist shouldBe List(2.0, 110.56, 266.32, 293.15)
  }

  "sample temperature" should "be converted" in {
    val sample = Kelvin(21.0)
    val c: Celsius = sample
    val f: Fahrenheit = sample
    val r: Rankine = sample
    val k: Kelvin = sample

    println(c)
    println(f)
    println(r)
    println(k)

    c.toDouble shouldBe (-252.15 +- 0.01)
    f.toDouble shouldBe (-421.87 +- 0.01)
    r.toDouble shouldBe (37.80 +- 0.01)
    c shouldBe f
    c shouldBe k
    c shouldBe r
  }

}
