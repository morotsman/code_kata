package kata.kata8

import org.scalatest._

class ComposedWordFinderSpec extends FlatSpec with Matchers {
  
  

  
  "columns" should "return columns of a line" in { 
    ComposedWordFinderExtendible.combinedWords(Set(), 2, 4) should be (List())
    ComposedWordFinderExtendible.combinedWords(Set("a","b"), 2, 2) should be (List())
    ComposedWordFinderExtendible.combinedWords(Set("a","b", "ab"), 2, 2) should be (List(List("a","b")))
    ComposedWordFinderExtendible.combinedWords(Set("a","b", "aabb"), 4, 4) should be (List())
    ComposedWordFinderExtendible.combinedWords(Set("d","b","a","c", "abcd"), 4, 4) should be (List(List("a","b","c","d")))
    ComposedWordFinderExtendible.combinedWords(Set("d","b","a","c","e", "abcd","abed","abcf"), 4, 4) should be (List(List("a","b","e","d"),List("a","b","c","d")))
    ComposedWordFinderExtendible.combinedWords(Set("aa","bb", "aabb"), 2, 4) should be (List(List("aa","bb"))) 
    ComposedWordFinderExtendible.combinedWords(Set("aa","bb", "aabb"), 2, 3) should be (List())   
  }
  

  
  
  
  
    
  
  
  
  

}