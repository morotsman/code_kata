package kata.kata8

import kata.KataUtil

object ComposedWordFinder1 {

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

object Main {

  def main(args: Array[String]): Unit = {
    ComposedWordFinder1.combinedSixLetterWords.foreach(ws => println(ws._1 + " + " + ws._2 + " => " + ws._3))

  }

}