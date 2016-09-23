package kata.kata4

import org.scalatest._

class DataMungingSpec extends FlatSpec with Matchers {
  
  

  
  "columns" should "return columns of a line" in { 
    DataMunging.columns("a") should be (Array("a"))
    DataMunging.columns(" a ") should be (Array("a"))
    DataMunging.columns("  a  ") should be (Array("a"))
    DataMunging.columns("ab") should be (Array("ab"))
    DataMunging.columns("a b") should be (Array("a","b"))
    DataMunging.columns("a  b") should be (Array("a","b"))
    DataMunging.columns("   9  86    32*   59       6  61.5       0.00         240  7.6 220  12  6.0  78 46 1018.6") should be (Array("9", "86", "32*", "59", "6", "61.5", "0.00", "240", "7.6", "220", "12", "6.0", "78", "46", "1018.6"))
  }
  

  
  
  
  
    
  
  
  
  

}