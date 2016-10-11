package kata.kata19

import kata.KataUtil

object WordChain {

  private def wordToInts(word: String): Array[Int] =
    word.
      toCharArray().
      map(_.toInt)

  private def differByOne(word1: String, word2: String): Boolean = 
    wordToInts(word1).zip(wordToInts(word2)).map(v => if (v._1 != v._2) 1 else 0).sum == 1
  
    

  def wordChain(dictionary: List[String])(from: String, to: String): List[List[String]] = ???

}


object Main {
  
  def main(args: Array[String]): Unit = {
    val dictionary = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.map(_.toLowerCase).filter(w => w.length == 3).toList
    }
    
    val wordChain = WordChain.wordChain(dictionary) _
    
    wordChain("cat","dog")
  }
  
  
}

