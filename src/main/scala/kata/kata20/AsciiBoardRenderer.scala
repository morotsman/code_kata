package kata.kata20



object AsciiBoardRenderer {
  
  private def formatSuite(s : Suite.Value) : String = 
    if(s == Suite.Clubs) {
      "\u2663"
    } else if(s == Suite.Diamonds) {
      "\u2666"
    } else if (s == Suite.Hearts) {
      "\u2764"
    } else {
      "\u2660"
    }
  
  private def formatPile(pile: Pile): List[String] = 
    pile.cards.map(c => if(c.hidden) "****" else (formatSuite(c.suite) + " " +  c.value))  
  
    
    
  def renderBoard(board: KlondikeBoard): Unit = {
    
    println("************************************************")
    println()
    println()
    
    if(board.discardPile.cards.length > 0) {
      print(formatPile(board.discardPile))
    } else {
      print("    ")
    }
    
    if(board.stockPile.cards.length > 0) {
      print(formatPile(board.stockPile).head)
    } else {
      print("    ")
    }
    
    board.foundationPiles.foreach { p => 
      if(p.cards.length > 0) {
        print(formatPile(p).head)
      } else {
        print("    ")
      }   
    }
    
    println()
    println()
    println("___________________________________________________")
    println()
    println()
    
    board.tableauPiles.zipWithIndex.foreach { case (p,index) => 
       if(p.cards.length > 0) {
        formatPile(p).foreach(println)
      } else {
        print("    ")
      }        
    }
    

    println()    
    println("*****************************************************")
    
  }

}