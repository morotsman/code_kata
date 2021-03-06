package kata.kata9

case class Checkout(rules: Map[String, Int], items: List[String] = List()) {

  assert(rules.size > 0)

  def scan(item: String): Checkout = {
    Checkout(rules, item :: items)
  }

  def total: Option[Int] =
    items.sorted.groupBy(identity).values.map(priceForItem).foldLeft(Some(0): Option[Int])(addOption)

  private def priceForItem(items: List[String]): Option[Int] = {

    def go(items: List[String], leftovers: List[String]): Option[Int] = {
      (items, applyRule(items.mkString)) match {
        case (Nil, _)        => None
        case (x :: xs, None) => go(items.drop(1), items.head :: leftovers)
        case (_, Some(p1)) =>
          if (leftovers.size > 0)
            priceForItem(leftovers).map(p2 => p1 + p2)
          else
            Some(p1)
      }
    }

    go(items, List())
  }

  private def applyRule(item: String): Option[Int] =
    rules.get(item)

  private def addOption(a: Option[Int], b: Option[Int]) =
    a.flatMap(v1 => b.map(v2 => v1 + v2))

}

