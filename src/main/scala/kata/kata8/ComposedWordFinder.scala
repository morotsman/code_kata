package kata.kata8

import kata.KataUtil
import scala.collection.parallel.immutable.ParSeq

object ComposedWordFinderReadable {

  def combinedSixLetterWords(source: String): List[(String, String, String)] = {
    val sixLetterWords = KataUtil.getFile(source) { s =>
      s.getLines.filter(_.length == 6).toSet //to lower case?
    }

    val lessThenSixLetterWords = KataUtil.getFile(source) { s =>
      s.getLines.filter(_.length < 6).toList
    }

    for (
      word1 <- lessThenSixLetterWords;
      word2 <- lessThenSixLetterWords;
      if word1.length + word2.length == 6 && word1 != word2 && sixLetterWords.contains(word1 + word2)
    ) yield (word1, word2, word1 + word2)

  }

}

object ComposedWordFinderFast {
  def combinedSixLetterWords(source: String): ParSeq[(String, String, String)] = {
    val sixLetterWords = KataUtil.getFile(source) { s =>
      s.getLines.filter(_.length == 6).toSet //to lower case?
    }

    val lessThenSixLetterWords = KataUtil.getFile(source) { s =>
      s.getLines.filter(_.length < 6).toList
    }

    for (
      word1 <- lessThenSixLetterWords.par;
      word2 <- lessThenSixLetterWords;
      if word1.length + word2.length == 6 && word1 != word2 && sixLetterWords.contains(word1 + word2)
    ) yield (word1, word2, word1 + word2)

  }
}

//my interpretation of the instructions was to produce a general solution i.e. different numbers of words and length of words 
object ComposedWordFinderExtendible {

  def combinedWords(source: String, nrOfWords: Int, lengthOfWord: Int): List[List[String]] = {

    val dictionary = KataUtil.getFile(source) { s =>
      s.getLines.filter(_.length == lengthOfWord).toSet //to lower case?
    }

    val words = KataUtil.getFile(source) { s =>
      s.getLines.filter(_.length < lengthOfWord).toList
    }

    def go(words: List[List[String]], wordList: List[String]): List[List[String]] = words match {
      case Nil => Nil
      case x :: Nil =>
        val word = wordList.mkString
        x.withFilter(w2 => word.length + w2.length == lengthOfWord).
          withFilter(w2 => !wordList.contains(w2)).
          withFilter(w2 => dictionary.contains(word + w2)).
          map(w2 => wordList :+ w2)
      case x :: xs =>
        x.par.
          withFilter(w1 => wordList.mkString.length + w1.length < lengthOfWord).
          flatMap(w1 => go(xs, wordList :+ w1)).toList
    }

    go(List.fill(nrOfWords)(words), List())

  }
}

object Main {

  def main(args: Array[String]): Unit = {

    val startTime1 = System.currentTimeMillis
    ComposedWordFinderReadable.combinedSixLetterWords("/wordlist.txt").foreach(w => println(w._1 + " + " + w._2 + " => " + w._3))
    val endTime1 = System.currentTimeMillis

    val startTime2 = System.currentTimeMillis
    ComposedWordFinderFast.combinedSixLetterWords("/wordlist.txt").foreach(w => println(w._1 + " + " + w._2 + " => " + w._3))
    val endTime2 = System.currentTimeMillis 

    val startTime3 = System.currentTimeMillis
    ComposedWordFinderExtendible.combinedWords("/wordlist.txt", 2, 6).foreach(r => println(r.mkString("+") + " => " + r.mkString))
    val endTime3 = System.currentTimeMillis

    
    println("time for solution1 (millis): " + (endTime1-startTime1))
    println("time for solution2 (millis): " + (endTime2 - startTime2))
    println("time for solution3 (millis): " + (endTime3 - startTime3))

  }

}