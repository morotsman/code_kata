package kata.kata6

import collection.mutable.HashMap
import scala.io.Source
import java.io.InputStream

import kata.KataUtil

object AnagramFinder {
  
  
  def main(args: Array[String]): Unit = {
    val anagrams = KataUtil.getFile("/anagramlist.txt") { s => s.getLines().toList.groupBy(_.sorted).filter(_._2.length > 1) } 
    val mostAnagrams = anagrams.reduceLeft((a,b) => if (a._2.length > b._2.length) a else b)
    val longestWord = anagrams.reduceLeft((a,b) => if (a._1.length > b._1.length) a else b)
    
    
    anagrams.foreach(println)
    
    println("**********************")
    println("Number of anagrams: " +anagrams.keySet.size)
    println("Most anagrams: " + mostAnagrams)
    println("Longest word: " + longestWord)  
  }

}