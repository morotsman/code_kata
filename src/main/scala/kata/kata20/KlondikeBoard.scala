package kata.kata20

case class KlondikeBoard(val stockPile: StockPile, discardPile: DiscardPile, tableauPiles: List[TableauPile], foundationPiles: List[FoundationPile]) extends Board {

  def legalMoves(): List[Move] = {

    def fillStockIfEmpty: List[Move] =
      if (stockPile.cards.length == 0 && discardPile.cards.length > 0) {
        List(Move(discardPile, stockPile, discardPile.cards.length))
      } else {
        List()
      }

    def takeCardFromStock: List[Move] =
      if (stockPile.cards.length > 0) {
        List(Move(stockPile, discardPile, 1))
      } else {
        List()
      }

    def moveToFoundationPile: List[Move] = {

      def suiteMatches(foundationTopCard: Option[Card], card: Card): Boolean =
        foundationTopCard.map(_.suite == card.suite).getOrElse(true)

      def isNextCard(foundationTopCard: Option[Card], card: Card): Boolean =
        foundationTopCard.map(_.value).getOrElse(0) == card.value - 1

      def go(piles: List[(FoundationPile)]): List[Move] = piles match {
        case Nil => List()
        case (targetPile) :: ps => {
          val moves = for (
            sourcePile <- (discardPile :: tableauPiles);
            if sourcePile.cards.length > 0 && suiteMatches(targetPile.cards.headOption, sourcePile.cards.head) && isNextCard(targetPile.cards.headOption, sourcePile.cards.head)
          ) yield Move(sourcePile, targetPile, 1)
          moves ::: go(ps)
        }
      }
      go(foundationPiles)
    }

    /**
     * Move the top card of the discard pile to one of the tableau piles.
     * This card must be one less in rank and opposite in color to the card at the top of the destination tableau.
     */

    def moveToTableauPile: List[Move] = {

      def go(piles: List[TableauPile]): List[Move] = piles match {
        case Nil => List()
        case targetPile :: ps => {
          val moves = for (
            sourcePile <- (discardPile :: foundationPiles);
            if sourcePile.cards.length > 0 && targetPile.cards.length > 0
              && sourcePile.cards.head.oppositeColor(targetPile.cards.head) && sourcePile.cards.head.isPrevCard(targetPile.cards.head)
          ) yield Move(sourcePile, targetPile, 1)
          moves ::: go(ps)
        }
      }

      go(tableauPiles)
    }

    /**
     * Move one or more cards from one tableau pile to another.
     * If multiple cards are moved, they must be a sequence ascending in rank and alternating in color.
     * The card moved (or the top of the sequence moved) must be one less in rank and opposite in color to the card at the top of the destination tableau.
     * If the move leaves a face-down card to the top of the original pile, turn it over.
     */
    def moveCardFromOneTableauToAnother: List[Move] = {

      def go(piles: List[TableauPile]): List[Move] = piles match {
        case Nil => List()
        case targetPile :: ps => {
          val moves = for (
            sourcePile <- tableauPiles if targetPile != sourcePile && sourcePile.cards.length > 0 && targetPile.cards.length > 0;
            (card: Card, index) <- sourcePile.cards.zipWithIndex if !card.hidden && card.oppositeColor(targetPile.cards.head) && card.isPrevCard(targetPile.cards.head)
          ) yield Move(sourcePile, targetPile, index + 1)
          moves ::: go(ps)
        }
      }

      go(tableauPiles)
    }

    /**
     * If a move leaves a tableau pile empty, an exposed King at the top of a tableau or discard pile,
     * or a sequence starting with a King on a tableau pile, may be moved to it.
     */
    def moveKingToEmptyTableau: List[Move] = {

      def go(piles: List[TableauPile]): List[Move] = piles match {
        case Nil => List()
        case targetPile :: ps => {
          val discardAndFoundationMoves = for (
            sourcePile <- discardPile :: foundationPiles;
            if targetPile.cards.length == 0 && sourcePile.cards.length > 0 && sourcePile.cards.head.value == 13
          ) yield Move(sourcePile, targetPile, 1)

          val tableauMoves = for (
            sourcePile <- tableauPiles if sourcePile != targetPile && targetPile.cards.length == 0 && sourcePile.cards.length > 0;
            (card: Card, index) <- sourcePile.cards.zipWithIndex;
            if !card.hidden && card.value == 13
          ) yield Move(sourcePile, targetPile, index + 1)

          tableauMoves ::: discardAndFoundationMoves ::: go(ps)
        }

      }

      go(tableauPiles)

    }

    fillStockIfEmpty ::: takeCardFromStock ::: moveToFoundationPile ::: moveToTableauPile ::: moveCardFromOneTableauToAnother ::: moveKingToEmptyTableau
  }

  def makeMove(move: Move): KlondikeBoard = {
    
    def replacePile[T](piles: List[T],from: T, to: T): List[T] = piles match {
      case Nil => Nil
      case (t::ts) => (if(t == from) to else t) :: replacePile(ts, from,to)
    }

    def go(move: Move): KlondikeBoard = move match {
      case Move(StockPile(from), DiscardPile(to), _) =>
        val newStock = StockPile(from.drop(1))
        val newDiscard = DiscardPile(from.take(1).map(c => Card(c.suite, c.value, false)) ::: to)
        KlondikeBoard(newStock, newDiscard, tableauPiles, foundationPiles)
      case Move(DiscardPile(from), StockPile(to), _) =>
        val newStock = StockPile(from.map(c => Card(c.suite, c.value, true)).reverse)
        val newDiscard = DiscardPile(Nil)
        KlondikeBoard(newStock, newDiscard, tableauPiles, foundationPiles)
      case Move(DiscardPile(from), TableauPile(index, to), _) =>
        val newDiscard = DiscardPile(from.drop(1))
        val newTableau = TableauPile(index, from.take(1) ::: to)
        KlondikeBoard(stockPile, newDiscard, replacePile(tableauPiles,TableauPile(index, to), newTableau), foundationPiles)
      case Move(DiscardPile(from), FoundationPile(index,to), _) =>
        val newDiscard = DiscardPile(from.drop(1))
        val newFoundationPile = FoundationPile(index, from.take(1) ::: to)
        KlondikeBoard(stockPile, newDiscard, tableauPiles, replacePile(foundationPiles, FoundationPile(index, to), newFoundationPile))
      case Move(TableauPile(indexFrom, from), FoundationPile(indexTo,to), _) =>
        val newTableauPile = if(from.length > 1){
          val newHead = from.drop(1).head 
          val tableauCards = Card(newHead.suite,newHead.value,false) ::from.drop(2) 
          TableauPile(indexFrom, tableauCards)
        } else {
          TableauPile(indexFrom)
        }          
        val newFoundationPile = FoundationPile(indexTo, from.take(1) ::: to)
        KlondikeBoard(stockPile, discardPile, replacePile(tableauPiles, TableauPile(indexFrom, from), newTableauPile), replacePile(foundationPiles, FoundationPile(indexTo, to), newFoundationPile))        
      case Move(TableauPile(indexFrom, from), TableauPile(indexTo,to), number) =>
        val newFrom = if(from.length >= number + 1) {
          val newHead = from.drop(number).head
          val tableauCards = Card(newHead.suite,newHead.value,false):: from.drop(number+1)
          TableauPile(indexFrom, tableauCards)
        } else {
          TableauPile(indexFrom)
        }
        val newTo = TableauPile(indexTo, from.take(number):::to)
        
        val piles = replacePile(tableauPiles, TableauPile(indexFrom, from), newFrom)
        val piles2 = replacePile(piles, TableauPile(indexTo, to), newTo)
          
        KlondikeBoard(stockPile,discardPile,piles2,foundationPiles)
      case Move(FoundationPile(indexFrom,from), TableauPile(indexTo,to),_) =>
        val newFoundation = FoundationPile(indexFrom,from.drop(1))
        val newTableau = TableauPile(indexTo, from.take(1) ::: to)
        KlondikeBoard(stockPile, discardPile, replacePile(tableauPiles,TableauPile(indexTo, to), newTableau), replacePile(foundationPiles, FoundationPile(indexFrom, from),newFoundation))
    }

    go(move)

  }

}




case object KlondikeBoard