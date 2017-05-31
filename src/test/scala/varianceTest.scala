import org.scalatest.{FlatSpec, Matchers}

class varianceTest extends FlatSpec with Matchers{


  val variance = Chapter4.variance(Seq(12.34, 12.34))



  variance shouldBe 0

}
