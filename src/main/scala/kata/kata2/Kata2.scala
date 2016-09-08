package kata.kata2

import scala.annotation.tailrec
import play.api.libs.iteratee._
import scala.concurrent._
import scala.concurrent.duration._

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
  
  //divide the array, not very effective I guess and also not tail recursive: "hello stack overflow"
  def sliceAndDiceChop(target: Int, source: Array[Int]) : Int = {
    
    //this solution is not tailrec
    def go(source: Array[Int]): Option[Int] = {
      if(source.length == 0) {
        return None
      }
      val middleIndex = source.length / 2
      if (source(middleIndex) == target) {
        Some(middleIndex)
      } else if(source(middleIndex) < target) {
        go(source.drop(middleIndex+1)).map { x => x +  middleIndex+1}
      } else {
        go(source.take(middleIndex))
      }
    }
    
    go(source).getOrElse(-1)
    
  }
  
  //divide the array, not very effective I guess, but tail recursive
  def sliceAndDiceChopTailRec(target: Int, source: Array[Int]) : Int = {
    
    @annotation.tailrec
    def go(source: Array[Int], acc:Int): Option[Int] = {
      if(source.length == 0) {
        return None
      }
      val middleIndex = source.length / 2
      if (source(middleIndex) == target) {
        Some(acc + middleIndex)
      } else if(source(middleIndex) < target) {
        go(source.drop(middleIndex+1), acc + middleIndex+1)
      } else {
        go(source.take(middleIndex),acc)
      }
    }
    
    go(source,0).getOrElse(-1)
    
  }  
  
  

  
  
  
}