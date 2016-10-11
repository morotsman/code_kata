package kata.kata19

import org.scalatest._

class CheckoutSpec extends FlatSpec with Matchers {


  "WordChain" should "find a path between two words if they are the same" in {
   
    //val dictionary = List("cat")
    //val wordChain = WordChain.wordChain(dictionary.toStream) _
    
    //wordChain("cat", "cat") should be(List(List("cat")))
   
  }
  
  "WordChain" should "find a path between two words" in {
    
    var dictionary = List("aaa","aba","abc","aca","acd")
    var wordChain = WordChain.wordChain(dictionary.toSet) _ 
    wordChain("aaa", "acd") should be(List(List("aaa","aca","acd")))
    
    /*
    var dictionary = List("cat","wat")
    var wordChain = WordChain.wordChain(dictionary) _ 
    wordChain("cat", "wat") should be(List(List("cat","wat")))
    
    
    
    dictionary = List("cat","sat","wat")
    wordChain = WordChain.wordChain(dictionary) _
    wordChain("cat", "wat") should be(List(List("cat", "sat", "wat"),List("cat","wat")))    
    
    
    dictionary = List("cat","not","wat")
    wordChain = WordChain.wordChain(dictionary) _ 
    wordChain("cat", "wat") should be(List(List("cat","wat")))  
    
    println("************************")
    
    dictionary = List("cat","cft","wat")
    wordChain = WordChain.wordChain(dictionary) _  
    wordChain("cat", "wat") should be(List(List("cat","wat")))  
    
    dictionary = List("cat","cot","cog", "dog")
    wordChain = WordChain.wordChain(dictionary) _
    wordChain("cat", "dog") should be(List(List("cat","cot","cog", "dog")))   
    */
  }  
  

  

  

}