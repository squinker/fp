package chapter7

import java.util.concurrent.{Callable, ExecutorService, ForkJoinPool, Future}

import org.scalatest.{FlatSpec, Matchers}

class Chapter7Tests extends FlatSpec with Matchers {


  behavior of "chap 7"


  it should "par sum " in {

    val result = chapter7.run(List(1,2,3))

    result(new ForkJoinPool).get() shouldBe 6
    // shouldBe chapter7.Par.unit(6)
  }

  it should "sum paramgraphs" in {

    chapter7.sumParagraphs(List("the thing", "one", "the other thing","four"))(new ForkJoinPool).get() shouldBe 7

  }

}