package kata.kata4

import org.scalatest._

class DataMungingSpec extends FlatSpec with Matchers {
  
  

  
  "columns" should "return columns of a line" in { 
    DataMunging.toColumns("a") should be (Array("a"))
    DataMunging.toColumns(" a ") should be (Array("a"))
    DataMunging.toColumns("  a  ") should be (Array("a"))
    DataMunging.toColumns("ab") should be (Array("ab"))
    DataMunging.toColumns("a b") should be (Array("a","b"))
    DataMunging.toColumns("a  b") should be (Array("a","b"))
    DataMunging.toColumns("   9  86    32*   59       6  61.5       0.00         240  7.6 220  12  6.0  78 46 1018.6") should be (Array("9", "86", "32*", "59", "6", "61.5", "0.00", "240", "7.6", "220", "12", "6.0", "78", "46", "1018.6"))
  }
  

  
  
  
  
    
  
  
  
  

}