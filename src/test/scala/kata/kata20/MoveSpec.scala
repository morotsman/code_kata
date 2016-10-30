package kata.kata20

import org.scalatest._
  
class MoveSpec extends FlatSpec with Matchers {


  def emptyFoundationsPiles: List[FoundationPile] = List(new FoundationPile(1,Nil), new FoundationPile(2,Nil), new FoundationPile(3,Nil), new FoundationPile(4,Nil))

  def emptyTableauPiles: List[TableauPile] = List(TableauPile(1,Nil), TableauPile(2,Nil), TableauPile(3,Nil), TableauPile(4,Nil), TableauPile(5,Nil), TableauPile(6,Nil), TableauPile(7,Nil))

  
  "A Move form the stockPile to the discardPile" should "add an open card to the discardPile" in {
    val hidden = true
    val notHidden = false
    
    var stockPile = StockPile()
    var discardPile = DiscardPile()
    var board = BoardBuilder().withDiscardPile(discardPile).withStockPile(stockPile).build
    var result = board.makeMove(Move(stockPile,discardPile,1))
    result.stockPile should be(stockPile)
    result.discardPile should be(discardPile)
    
    stockPile = StockPile(Card(Suite.Clubs,12,hidden))
    discardPile = DiscardPile()
    board = BoardBuilder().withDiscardPile(discardPile).withStockPile(stockPile).build
    result = board.makeMove(Move(stockPile,discardPile,1))
    result.stockPile should be(StockPile())
    result.discardPile should be(DiscardPile(Card(Suite.Clubs,12,notHidden)))   
    
    stockPile = StockPile(Card(Suite.Clubs,12,hidden),Card(Suite.Diamonds,4,hidden))
    discardPile = DiscardPile(Nil)
    board = BoardBuilder().withDiscardPile(discardPile).withStockPile(stockPile).build
    result = board.makeMove(Move(stockPile,discardPile,1))
    result.stockPile should be(StockPile(Card(Suite.Diamonds,4,hidden)))
    result.discardPile should be(DiscardPile(Card(Suite.Clubs,12,notHidden)))     
    
    stockPile = StockPile(Card(Suite.Clubs,12,hidden),Card(Suite.Diamonds,4,hidden))
    discardPile = DiscardPile(Card(Suite.Hearts,2,notHidden))
    board = BoardBuilder().withDiscardPile(discardPile).withStockPile(stockPile).build
    result = board.makeMove(Move(stockPile,discardPile,1))
    result.stockPile should be(StockPile(Card(Suite.Diamonds,4,hidden)))
    result.discardPile should be(DiscardPile(Card(Suite.Clubs,12,notHidden),Card(Suite.Hearts,2,notHidden)))      
  } 
  
 "A Move form the discardPile to the stockPile" should "add all cards in the discardPile to the stockPile hidden" in {
    val hidden = true
    val notHidden = false
      
    var stockPile = StockPile()
    var discardPile = DiscardPile(Card(Suite.Clubs,12,notHidden))
    var board = BoardBuilder().withDiscardPile(discardPile).withStockPile(stockPile).build
    var result = board.makeMove(Move(discardPile,stockPile,1))
    result.stockPile should be(StockPile(Card(Suite.Clubs,12,hidden)))
    result.discardPile should be(DiscardPile())   
    
    stockPile = StockPile()
    discardPile = DiscardPile(Card(Suite.Clubs,12,notHidden),Card(Suite.Hearts,7,notHidden),Card(Suite.Diamonds,4,notHidden))
    board = BoardBuilder().withDiscardPile(discardPile).withStockPile(stockPile).build
    result = board.makeMove(Move(discardPile,stockPile,3))
    result.stockPile should be(StockPile(Card(Suite.Diamonds,4,hidden), Card(Suite.Hearts,7,hidden),Card(Suite.Clubs,12,hidden)))
    result.discardPile should be(DiscardPile())     
           
  }   
 
 "A Move form the discardPile to a tableauPile" should "add a card to the tableauPile" in {
    val hidden = true
    val notHidden = false
      
    var discardPile = DiscardPile(Card(Suite.Clubs,7,notHidden))
    var tableauPile = TableauPile(1,Card(Suite.Hearts,8,notHidden))
    var board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(0,tableauPile).build
    var result = board.makeMove(Move(discardPile,tableauPile,1))
    result.tableauPiles.head should be(TableauPile(1,Card(Suite.Clubs,7,notHidden),Card(Suite.Hearts,8,notHidden)))
    result.discardPile should be(DiscardPile()) 
    
    discardPile = DiscardPile(Card(Suite.Clubs,7,notHidden),Card(Suite.Clubs,3,notHidden))
    tableauPile = TableauPile(1,Card(Suite.Hearts,8,notHidden))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(0,tableauPile).build
    result = board.makeMove(Move(discardPile,tableauPile,1))
    result.tableauPiles.head should be(TableauPile(1,Card(Suite.Clubs,7,notHidden),Card(Suite.Hearts,8,notHidden)))
    result.discardPile should be(DiscardPile(Card(Suite.Clubs,3,notHidden)))    
    
    discardPile = DiscardPile(Card(Suite.Clubs,7,notHidden),Card(Suite.Clubs,3,notHidden))
    tableauPile = TableauPile(1,Card(Suite.Hearts,8,notHidden),Card(Suite.Hearts,3,hidden))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(0,tableauPile).build
    result = board.makeMove(Move(discardPile,tableauPile,1))
    result.tableauPiles.head should be(TableauPile(1,Card(Suite.Clubs,7,notHidden),Card(Suite.Hearts,8,notHidden),Card(Suite.Hearts,3,hidden)))
    result.discardPile should be(DiscardPile(Card(Suite.Clubs,3,notHidden)))       
 
    discardPile = DiscardPile(Card(Suite.Clubs,7,notHidden))
    var tableauPile1 = TableauPile(2,Card(Suite.Diamonds,8,notHidden))  
    var tableauPile2 = TableauPile(4,Card(Suite.Hearts,8,notHidden))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(1,tableauPile1).withTableauPile(3,tableauPile2).build
    result = board.makeMove(Move(discardPile,tableauPile2,1))
    result.tableauPiles.drop(3).head should be(TableauPile(4,Card(Suite.Clubs,7,notHidden),Card(Suite.Hearts,8,notHidden)))
    result.discardPile should be(DiscardPile())
    
  }  
 
  "A Move form the discardPile to a founadtionPile" should "add a card to the foundationPile" in {
    val hidden = true
    val notHidden = false
      
    var discardPile = DiscardPile(Card(Suite.Clubs,1,notHidden))
    var foundationPile = FoundationPile(1)
    var board = BoardBuilder().withDiscardPile(discardPile).withFoundationPile(0,foundationPile).build
    var result = board.makeMove(Move(discardPile,foundationPile,1))
    result.foundationPiles.head should be(FoundationPile(1,Card(Suite.Clubs,1,notHidden)))
    result.discardPile should be(DiscardPile()) 
    
    discardPile = DiscardPile(Card(Suite.Clubs,1,notHidden),Card(Suite.Clubs,2,notHidden))
    foundationPile = FoundationPile(1)
    board = BoardBuilder().withDiscardPile(discardPile).withFoundationPile(0,foundationPile).build
    result = board.makeMove(Move(discardPile,foundationPile,1))
    result.foundationPiles.head should be(FoundationPile(1,Card(Suite.Clubs,1,notHidden)))
    result.discardPile should be(DiscardPile(Card(Suite.Clubs,2,notHidden)))  
    
    discardPile = DiscardPile(Card(Suite.Clubs,2,notHidden))
    foundationPile = FoundationPile(1,Card(Suite.Clubs,1,notHidden))
    board = BoardBuilder().withDiscardPile(discardPile).withFoundationPile(0,foundationPile).build
    result = board.makeMove(Move(discardPile,foundationPile,1))
    result.foundationPiles.head should be(FoundationPile(1,Card(Suite.Clubs,2,notHidden), Card(Suite.Clubs,1,notHidden)))
    result.discardPile should be(DiscardPile())    
  }
  

}