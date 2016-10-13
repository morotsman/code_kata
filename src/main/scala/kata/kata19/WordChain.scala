package kata.kata19

import kata.KataUtil


case class Word(val word:String, val path: List[String])

object WordChain {

  private def wordToInts(word: String): Array[Int] =
    word.toCharArray().map(_.toInt)

  private def differByOne(word1: String, word2: String): Boolean = 
    wordToInts(word1).zip(wordToInts(word2)).map(v => if (v._1 != v._2) 1 else 0).sum == 1
  
    

  def wordChain(totalDictionary: Set[String])(from: String, to: String): List[String] = {
    assert(from.length == to.length, "The words in the chain has different length.")
    assert(totalDictionary.size>0, "The dictionary must contain words.")
    if(from == to) return List(from)
    
    val dictionary = totalDictionary.filter(w => w.length == from.length)

    def getSuspects(from:Word,investigated:Set[String]): Set[Word] = {
      val suspects = dictionary.filter(w => differByOne(from.word, w))
      for (
          suspect <- suspects;
          if !investigated.contains(suspect);
          newWord = Word(suspect,suspect::from.path)          
        ) yield newWord
    }
    
    def go(suspects: Set[Word], alreadyInvestigated: Set[String]): List[String] = {
      
      val target = suspects.toList.find(w => w.word == to )
      
      if(target != None){
        target.get.path.reverse 
      }else{
        val investigated = alreadyInvestigated union suspects.map(_.word)
        val newSuspects = for (
          suspect <- suspects;
          newSuspect <- getSuspects(suspect,investigated)
        ) yield newSuspect 
        go(newSuspects, investigated)
      }
    }

    go(getSuspects(Word(from,List(from)), Set(from)), Set(from))
  }

}


object Main {
  
  def main(args: Array[String]): Unit = {
    val dictionary = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.map(_.toLowerCase).toSet
    }
    
    val wordChain = WordChain.wordChain(dictionary) _
    
    val startTime = System.currentTimeMillis
    println(wordChain("cat","dog"))
    val endTime = System.currentTimeMillis
    println("Time in millis: " + (endTime-startTime))
  }
  
  
}
