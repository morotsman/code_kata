package kata.kata8

import kata.KataUtil
import scala.collection.parallel.immutable.ParSet

object ComposedWordFinderReadable {

  def combinedSixLetterWords: Set[(String,String,String)] = {
    val sixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length == 6).toSet //to lower case?
    }

    val lessThenSixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length < 6).toSet
    }  

    for (
      word1 <- lessThenSixLetterWords;
      word2 <- lessThenSixLetterWords;
      if word1.length + word2.length == 6 && sixLetterWords.contains(word1 + word2)
    ) yield (word1, word2, word1 + word2)
    
  }

}

object ComposedWordFinderFast {
  def combinedSixLetterWords: ParSet[(String,String,String)] = {
    val sixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length == 6).toSet //to lower case?
    }

    val lessThenSixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length < 6).toSet
    }
    val endTime1 = System.currentTimeMillis

    for (
      word1 <- lessThenSixLetterWords.par;
      word2 <- lessThenSixLetterWords;
      if word1.length + word2.length == 6 && sixLetterWords.contains(word1 + word2)
    ) yield (word1, word2, word1 + word2)

  }
}

object Main {

  def main(args: Array[String]): Unit = {
    
    val startTime1 = System.currentTimeMillis
    ComposedWordFinderReadable.combinedSixLetterWords.foreach(w => println(w._1 + " + " + w._2 + " => " + w._3))
    val endTime1 = System.currentTimeMillis
    
    
    val startTime2 = System.currentTimeMillis
    ComposedWordFinderFast.combinedSixLetterWords.foreach(w => println(w._1 + " + " + w._2 + " => " + w._3))
    val endTime2 = System.currentTimeMillis
    
    println("time for solution1 (millis): " + (endTime1-startTime1))
    println("time for solution2 (millis): " + (endTime2-startTime2))    
    

  }

}