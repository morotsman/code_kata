package kata.kata9

import org.scalatest._

class CheckoutSpec extends FlatSpec with Matchers {

  def price(rules: Map[String, Int])(items: String): Option[Int] =
    items.split("").foldLeft(Checkout(rules))(_.scan(_)).total

  "Checkout" should "with no rules should result in exception" in {
    try {
      Checkout(Map()).scan("A").total should be(0)
    } catch {
      case e: AssertionError => "OK"
      case _: Throwable      => fail
    }

  }

  "Checkout with failures" should "should result in None" in {
    val rules = Map(
      "A" -> 50,
      "B" -> 30,
      "C" -> 20,
      "D" -> 15,
      "AAA" -> 130,
      "BB" -> 45)

    val checkout = price(rules) _

    checkout("") should be(None)
    checkout("E") should be(None)
    checkout("AEB") should be(None)
  }

  "Checkout without failures" should "should result in a total price" in {
    val rules = Map(
      "A" -> 50,
      "B" -> 30,
      "C" -> 20,
      "D" -> 15,
      "AAA" -> 130,
      "BB" -> 45)

    val checkout = price(rules) _

    checkout("A") should be(Some(50))
    checkout("AB") should be(Some(80))
    checkout("CDBA") should be(Some(115))

    checkout("AA") should be(Some(100))
    checkout("AAA") should be(Some(130))
    checkout("AAAA") should be(Some(180))
    checkout("AAAAA") should be(Some(230))
    checkout("AAAAAA") should be(Some(260))

    checkout("AAAB") should be(Some(160))
    checkout("AAABB") should be(Some(175))
    checkout("AAABBD") should be(Some(190))
    checkout("DABABA") should be(Some(190))
  }

}