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

  def renderTableauRow(cards: List[Option[Card]]): String =
    cards.map(c => c.map(renderCard).getOrElse("")).mkString("       ")

  private def renderTableau(piles: List[TableauPile]): List[String] = {
    if (piles.map(p => p.cards.length).exists(_ > 0)) {
      val cards = for (
        pile <- piles;
        card = pile.cards.headOption
      ) yield card

      val newTableaus = for (
        pile <- piles
      ) yield (TableauPile(pile.index, pile.cards.drop(1)))

      renderTableauRow(cards) :: renderTableau(newTableaus)

    } else Nil

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
      print(renderPile(board.discardPile).head + "   ")
    } else {
      print("    ")
    }

    if (board.stockPile.cards.length > 0) {
      print(renderPile(board.stockPile).head + "    ")
    } else {
      print("    ")
    }

    board.foundationPiles.foreach { p =>
      if (p.cards.length > 0) {
        print(renderPile(p).head + "    ")
      } else {
        print("    ")
      }
    }

    renderDivider.foreach(println)

    renderTableau(board.tableauPiles).foreach(println)

    renderEnd.foreach(println)

  }

}