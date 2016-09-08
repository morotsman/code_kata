package kata.kata2

import org.scalatest._

class ExampleSpec extends FlatSpec with Matchers {
  
  def testExecutor(testSubject : (Int,Array[Int]) => Int) : Unit = {
    testSubject(3, Array()) should be (-1)
    testSubject(3, Array(1)) should be (-1)
    testSubject(1, Array(1)) should be (0)
    
    
    testSubject(1, Array(1,3,5)) should be (0)
    testSubject(3, Array(1,3,5)) should be (1)
    testSubject(5, Array(1,3,5)) should be (2)
    testSubject(0, Array(1,3,5)) should be (-1)
    testSubject(2, Array(1,3,5)) should be (-1)
    testSubject(4, Array(1,3,5)) should be (-1)
    testSubject(6, Array(1,3,5)) should be (-1) 
    
    
    testSubject(1, Array(1,3,5,7)) should be (0)
    testSubject(3, Array(1,3,5,7)) should be (1)
    testSubject(5, Array(1,3,5,7)) should be (2)  
    testSubject(7, Array(1,3,5,7)) should be (3)  
    testSubject(0, Array(1,3,5,7)) should be (-1)
    testSubject(2, Array(1,3,5,7)) should be (-1)
    testSubject(4, Array(1,3,5,7)) should be (-1)  
    testSubject(6, Array(1,3,5,7)) should be (-1)  
    testSubject(8, Array(1,3,5,7)) should be (-1) 
    
    testSubject(1, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (0)
    testSubject(3, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (1)
    testSubject(5, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (2)
    testSubject(7, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (3)
    testSubject(9, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (4)
    testSubject(11, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (5)
    testSubject(13, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (6)
    testSubject(15, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (7)
    testSubject(17, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (8)
    testSubject(19, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (9)
    testSubject(21, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (10)
    testSubject(0, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(2, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(4, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(6, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(8, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(10, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(12, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(14, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(16, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(18, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(20, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
    testSubject(22, Array(1,3,5,7,9,11,13,15,17,19,21)) should be (-1)
   
    
  }

  
  "A recurcive binary search" should "find elements" in { 
    testExecutor(Kata2.recursiveChop)
  }
  
  "A iteratice binary search" should "find elements" in { 
    testExecutor(Kata2.iterativeChop)
  }  
  
  
  "A sliceAndDice binary search" should "find elements" in { 
    testExecutor(Kata2.sliceAndDiceChop)
  }   
  
  "A tail rec sliceAndDice binary search" should "find elements" in { 
    testExecutor(Kata2.sliceAndDiceChopTailRec)
  } 
    
  
  
  
  

}