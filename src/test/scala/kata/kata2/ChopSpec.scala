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
  }

  "A recurcive binary search" should "find elements" in { 
    testExecutor(Kata2.recursiveChop)
  }
  
  "A iteratice binary search" should "find elements" in { 
    testExecutor(Kata2.iterativeChop)
  }  

}