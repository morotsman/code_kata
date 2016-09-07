package kata.kata2

import scala.annotation.tailrec

object Kata2 {
  
  def recursiveChop(target: Int, source: Array[Int]): Int = { 
    if(source.length == 0) {
      return -1
    }
    
    @annotation.tailrec
    def go(startIndex: Int, endIndex: Int) : Int = {
      if (startIndex > endIndex) {
        return -1
      }
      val middleIndex = startIndex + (endIndex - startIndex)/2
      if (source(middleIndex) == target) {
        middleIndex
      } else if(source(middleIndex) < target) {
        go(middleIndex+1,endIndex)
      } else {
        go(startIndex,middleIndex-1)
      }
    }
    
    go(0,source.length-1) 
  }
  
  def iterativeChop(target: Int, source: Array[Int]) : Int = {
    if(source.length == 0) {
      return -1
    }
    var startIndex = 0
    var endIndex = source.length - 1
    while(startIndex <= endIndex) {
      val middleIndex = startIndex + (endIndex - startIndex)/2
      if (source(middleIndex) == target) {
        return middleIndex
      } else if(source(middleIndex) < target) {
        startIndex = middleIndex + 1
      } else {
        endIndex = middleIndex - 1
      }
    }
    return -1
  }
  
  //divide the array
  def sliceAndDiceChop(target: Int, source: Array[Int]) : Int = {
    if(source.length == 0) {
      return -1
    }
    
    def go(): Int = ???
    
    go()
    
  }
  
  
  
  
  
}