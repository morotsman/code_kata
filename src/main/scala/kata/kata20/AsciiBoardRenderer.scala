package kata.kata20

object AsciiBoardRenderer {

  private def renderSuite(s: Suite.Value): String =
    if (s == Suite.Clubs) {
      "\u2663"
    } else if (s == Suite.Diamonds) {
      "\u2666"
    } else if (s == Suite.Hearts) {
      "\u2764"
    } else {
      "\u2660"
    }

  private def renderCard(card: Card): String =
    if (card.hidden) "***" else ((if (card.value < 10) " " else "") + renderSuite(card.suite) + card.value)

  private def renderPile(pile: Pile): List[String] =
    pile.cards.map(renderCard)

  def renderTableauRow(cards: Card*): String =
    cards.map(renderCard).mkString("       ")

  private def renderTableau(piles: List[TableauPile]): List[String] = piles match {
    case Nil => Nil
    case List(TableauPile(a :: as), TableauPile(b :: bs), TableauPile(c :: cs), TableauPile(d :: ds), TableauPile(e :: es), TableauPile(f :: fs), TableauPile(g :: gs)) =>
      renderTableauRow(a, b, c, d, e, f, g) ::
        renderTableau(List(TableauPile(as), TableauPile(bs), TableauPile(cs), TableauPile(ds), TableauPile(es), TableauPile(fs)))
    case List(TableauPile(a :: as), TableauPile(b :: bs), TableauPile(c :: cs), TableauPile(d :: ds), TableauPile(e :: es), TableauPile(f :: fs)) =>
      renderTableauRow(a, b, c, d, e, f) ::
        renderTableau(List(TableauPile(as), TableauPile(bs), TableauPile(cs), TableauPile(ds), TableauPile(es)))
    case List(TableauPile(a :: as), TableauPile(b :: bs), TableauPile(c :: cs), TableauPile(d :: ds), TableauPile(e :: es)) =>
      renderTableauRow(a, b, c, d, e) ::
        renderTableau(List(TableauPile(as), TableauPile(bs), TableauPile(cs), TableauPile(ds)))
    case List(TableauPile(a :: as), TableauPile(b :: bs), TableauPile(c :: cs), TableauPile(d :: ds)) =>
      renderTableauRow(a, b, c, d) ::
        renderTableau(List(TableauPile(as), TableauPile(bs), TableauPile(cs)))
    case List(TableauPile(a :: as), TableauPile(b :: bs), TableauPile(c :: cs)) =>
      renderTableauRow(a, b, c) ::
        renderTableau(List(TableauPile(as), TableauPile(bs)))
    case List(TableauPile(a :: as), TableauPile(b :: bs)) =>
      renderTableauRow(a, b) ::
        renderTableau(List(TableauPile(as)))
    case List(TableauPile(a :: as)) =>
      renderTableauRow(a) ::
        renderTableau(List())

  }

  def renderTop(): List[String] =
    List(
      "******************************************************************",
      "",
      "")

  def renderEnd(): List[String] =
    List(
      "",
      "",
      "******************************************************************",
      "",
      "")

  def renderDivider(): List[String] =
    List(
      "",
      "",
      "__________________________________________________________________",
      "",
      "")

  def renderBoard(board: KlondikeBoard): Unit = {

    renderTop.foreach(println)

    if (board.discardPile.cards.length > 0) {
      print(renderPile(board.discardPile))
    } else {
      print("    ")
    }

    if (board.stockPile.cards.length > 0) {
      print(renderPile(board.stockPile).head)
    } else {
      print("    ")
    }

    board.foundationPiles.foreach { p =>
      if (p.cards.length > 0) {
        print(renderPile(p).head)
      } else {
        print("    ")
      }
    }

    renderDivider.foreach(println)

    renderTableau(board.tableauPiles).foreach(println)

    renderEnd.foreach(println)

  }

}