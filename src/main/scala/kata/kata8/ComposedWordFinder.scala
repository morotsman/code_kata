package kata.kata8

import kata.KataUtil
import scala.collection.parallel.immutable.ParSeq

object ComposedWordFinderReadable {

  def combinedSixLetterWords(sourceDictionary: Set[String]): List[List[String]] = {
    val sixLetterWords = sourceDictionary.filter(_.length == 6)  
    val lessThenSixLetterWords = sourceDictionary.filter(_.length < 6).toList

    for (
      word1 <- lessThenSixLetterWords;
      word2 <- lessThenSixLetterWords;
      if word1.length + word2.length == 6 && word1 != word2 && sixLetterWords.contains(word1 + word2)
    ) yield (List(word1, word2))

  }

}

object ComposedWordFinderFast {
  def combinedSixLetterWords(sourceDictionary: Set[String]): ParSeq[List[String]] = {
    val sixLetterWords = sourceDictionary.filter(_.length == 6)  
    val lessThenSixLetterWords = sourceDictionary.filter(_.length < 6).toList

    for (
      word1 <- lessThenSixLetterWords.par;
      word2 <- lessThenSixLetterWords;
      if word1.length + word2.length == 6 && word1 != word2 && sixLetterWords.contains(word1 + word2)
    ) yield List(word1, word2)

  }
}

//my interpretation of the instructions was to produce a general solution i.e. different numbers of words and length of words 
object ComposedWordFinderExtendible {

  def combinedWords(sourceDictionary: Set[String], nrOfWords: Int, lengthOfWord: Int): List[List[String]] = {

    val dictionary: Set[String] = sourceDictionary.filter(_.length == lengthOfWord)
    val words: List[String] = sourceDictionary.filter(_.length < lengthOfWord).toList

    def go(level:Int,words: List[List[String]], wordList: List[String]): List[List[String]] = words match {
      case Nil => if(dictionary.contains(wordList.mkString)) List(wordList) else List()
      case x :: xs =>
        val word = wordList.mkString
        (if(level==0) x.par else x).
          withFilter(w => if(xs != Nil) word.length + w.length < lengthOfWord else word.length + w.length == lengthOfWord).
          withFilter(w => !wordList.contains(w)).
          flatMap(w => go(level+1,xs, wordList :+ w)).toList
    }

    go(0,List.fill(nrOfWords)(words), List())

  }
}

object Main {

  def main(args: Array[String]): Unit = {
    
    val dictionary = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(w => w.length > 1).toSet 
    } + "I" + "O" + "a" + "A"

    val startTime1 = System.currentTimeMillis
    val result1 = ComposedWordFinderReadable.combinedSixLetterWords(dictionary)
    //println(result1.size)
    result1.foreach(r => println(r.mkString("+") + " => " + r.mkString))
    val endTime1 = System.currentTimeMillis

    val startTime2 = System.currentTimeMillis
    val result2 = ComposedWordFinderFast.combinedSixLetterWords(dictionary)
    //println(result2.size)
    result2.foreach(r => println(r.mkString("+") + " => " + r.mkString))
    val endTime2 = System.currentTimeMillis 

    val startTime3 = System.currentTimeMillis
    val result3 = ComposedWordFinderExtendible.combinedWords(dictionary, 2, 6)
    //println(result3.size)
    result3.foreach(r => println(r.mkString("+") + " => " + r.mkString))
    val endTime3 = System.currentTimeMillis

    
    println("time for solution1 (millis): " + (endTime1-startTime1))
    println("time for solution2 (millis): " + (endTime2 - startTime2))
    println("time for solution3 (millis): " + (endTime3 - startTime3))

  }

}