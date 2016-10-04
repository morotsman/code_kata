package kata.kata8

import kata.KataUtil
import scala.collection.parallel.immutable.ParSeq

object ComposedWordFinderReadable {

  def combinedSixLetterWords: List[(String, String, String)] = {
    val sixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length == 6).toSet //to lower case?
    }

    val lessThenSixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length < 6).toList
    }

    for (
      word1 <- lessThenSixLetterWords;
      word2 <- lessThenSixLetterWords;
      if word1.length + word2.length == 6 && sixLetterWords.contains(word1 + word2)
    ) yield (word1, word2, word1 + word2)

  }

}

object ComposedWordFinderFast {
  def combinedSixLetterWords: ParSeq[(String, String, String)] = {
    val sixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length == 6).toSet //to lower case?
    }

    val lessThenSixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length < 6).toList
    }

    for (
      word1 <- lessThenSixLetterWords.par;
      word2 <- lessThenSixLetterWords;
      if word1.length + word2.length == 6 && sixLetterWords.contains(word1 + word2)
    ) yield (word1, word2, word1 + word2)

  }
}

object ComposedWordFinderExtendible {

  def combinedWords: List[(List[String], String)] = {
    val dictionary = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length == 6).toSet //to lower case?
    }

    val lessThenSixLetterWords = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.filter(_.length < 6).toList
    }


    def go(words: List[List[String]]): List[(List[String], String)] =
      lessThenSixLetterWords.flatMap(w1 =>
        lessThenSixLetterWords.
          withFilter(w2 => w1.length + w2.length == 6).
          withFilter(w2 => dictionary.contains(w1 + w2)).
          map(w2 => (List(w1, w2), w1 + w2)))

    go(List.fill(2)(lessThenSixLetterWords))

  }
}

object Main {

  def main(args: Array[String]): Unit = {

    //val startTime1 = System.currentTimeMillis
    //ComposedWordFinderReadable.combinedSixLetterWords.foreach(w => println(w._1 + " + " + w._2 + " => " + w._3))
    //val endTime1 = System.currentTimeMillis

    val startTime2 = System.currentTimeMillis
    ComposedWordFinderFast.combinedSixLetterWords //.foreach(w => println(w._1 + " + " + w._2 + " => " + w._3))
    val endTime2 = System.currentTimeMillis

    println("time for solution2 (millis): " + (endTime2 - startTime2))

    val startTime3 = System.currentTimeMillis
    ComposedWordFinderExtendible.combinedWords //.foreach(r => r._1.mkString("+") + " => " + r._2)
    val endTime3 = System.currentTimeMillis

    //println("time for solution1 (millis): " + (endTime1-startTime1))

    println("time for solution3 (millis): " + (endTime3 - startTime3))

  }

}