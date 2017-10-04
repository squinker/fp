package chapter9

object Parsers {

  trait Parsers[ParseError, Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]
    def char(c: Char): Parser[Char] =
      string(c.toString) map (_.charAt(0))

    def many[A](p: Parser[A]): Parser[List[A]]
    def map[A,B](p: Parser[A])(f: A => B) : Parser[B]

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    //val numA = char('a').many



    val testOr = "ab" | "ba"

    implicit def string(s: String): Parser[String]



    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def many = self.many(p)



      def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
      def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    }
  }

}
