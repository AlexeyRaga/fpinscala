package fpinscala.parsing

import java.util.regex._
import scala.language.{implicitConversions, higherKinds}
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeed[A](a: A) = string("").map((_: String) => a)

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def many[A](p: Parser[A]): Parser[List[A]] =
    or(many1(p), succeed(List.empty))

  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map { x => f(x._1, x._2) }

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[A, B](p2: Parser[B]) = self.product(p, p2)
    def product[A, B](p2: Parser[B]) = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity[A]))(in)

    def productAssociativeLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop =
      forAll(in) {s =>
        val rp1 = ((p1 ** p2) ** p3).map { case ((a, b), c) => (a, b, c) }
        val rp2 = (p1 ** (p2 ** p3)).map { case (a, (b, c)) => (a, b, c) }

        run(rp1)(s) == run(rp2)(s)
      }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}