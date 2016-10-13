package kata.kata19

import org.scalatest._

class CheckoutSpec extends FlatSpec with Matchers {


  "WordChain" should "find a path between two words if they are the same" in {
   
    val dictionary = Set("cat")
    val wordChain = WordChain.wordChain(dictionary) _
    
    wordChain("cat", "cat") should be(List("cat"))
   
  }
  
  "WordChain" should "find a path between two words" in {
    
    var dictionary = Set("aaa","aba","abc","aca","acd")
    var wordChain = WordChain.wordChain(dictionary.toSet) _ 
    wordChain("aaa", "acd") should be(List("aaa","aca","acd"))
    
    println("*********")
    
    dictionary = Set("cat","wat")
    wordChain = WordChain.wordChain(dictionary) _ 
    wordChain("cat", "wat") should be(List("cat","wat"))
     
    dictionary = Set("cat","sat","wat")
    wordChain = WordChain.wordChain(dictionary) _
    wordChain("cat", "wat") should be(List("cat","wat"))    
    
    dictionary = Set("cat","not","wat")
    wordChain = WordChain.wordChain(dictionary) _ 
    wordChain("cat", "wat") should be(List("cat","wat")) 
    
    dictionary = Set("cat","cft","wat")
    wordChain = WordChain.wordChain(dictionary) _  
    wordChain("cat", "wat") should be(List("cat","wat"))  
    
    dictionary = Set("cat","cot","cog", "dog")
    wordChain = WordChain.wordChain(dictionary) _
    wordChain("cat", "dog") should be(List("cat","cot","cog", "dog"))   
    
  }  
  

  

  

}