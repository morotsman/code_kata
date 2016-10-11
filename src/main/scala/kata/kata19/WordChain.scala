package kata.kata19

import kata.KataUtil

object WordChain {

  private def wordToInts(word: String): Array[Int] =
    word.
      toCharArray().
      map(_.toInt)

  private def differByOne(word1: String, word2: String): Boolean = 
    wordToInts(word1).zip(wordToInts(word2)).map(v => if (v._1 != v._2) 1 else 0).sum == 1
  
    

  def wordChain(dictionary: Set[String])(from: String, to: String): List[String] = {

    def getSuspects(word:String,investigated:Set[String]): Set[String] = {
      for (
          suspect <- dictionary.filter(w => differByOne(word, w));
          if !investigated.contains(suspect)
        ) yield suspect
    }
    
    def go(path: List[String], suspects: Set[String], alreadyInvestigated: Set[String]): List[String] = { 
      println(to)
      println(suspects.size)
      if(suspects.contains(to)){
        println("hittade")
        path 
      }else{
        val investigated = alreadyInvestigated union suspects
        val newSuspects = for (
          suspect <- suspects;
          newSuspect <- getSuspects(suspect,investigated)
        ) yield newSuspect
          
        go(path, newSuspects, investigated)
      }
    }

    go(List(from),getSuspects(from,Set(from)), Set(from))
  }

}


object Main {
  
  def main(args: Array[String]): Unit = {
    val dictionary = KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines.map(_.toLowerCase).filter(w => w.length == 4).toSet
    }
    
    val wordChain = WordChain.wordChain(dictionary) _
    
    val startTime = System.currentTimeMillis
    wordChain("ruby","code")
    val endTime = System.currentTimeMillis
    println("Time in millis: " + (endTime-startTime))
  }
  
  
}

