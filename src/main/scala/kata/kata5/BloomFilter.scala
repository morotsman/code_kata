package kata.kata5

import java.security.MessageDigest
import java.math.BigInteger
import scala.io.Source
import scala.io.StdIn
import java.io.InputStream
import kata._

class BloomFilter(size: Int, nrHashes: Int) {
  require(nrHashes<=16, "Max 16 hashes may be used with this implementation.")  

  val bitmap: Array[Boolean] = new Array(size)
  val partsPerSubArray = numberDivider(16, nrHashes)

  def addKey(s: String): Unit =
    createHashes(s).foreach(bitmap(_) = true)

  def hasKey(s: String): Boolean =
    createHashes(s).forall(bitmap(_) == true)

  def ratioPrecentage(): Float =
    (bitmap.filter(_ == true).size).toFloat / size.toFloat * 100
    
  private def numberDivider(number:Int, parts:Int): List[Int] = {
    val itemsPerPart = List.fill(parts)(number/parts)
    val leftovers = List.fill(number % parts)(1)
    itemsPerPart.zipAll(leftovers,0,0).map(a => a._1 + a._2)
  }
 
  private def createHashes(s: String): List[Int] = {
    splitArray(md5(s), nrHashes).map(a => (new BigInteger(a)).mod(new BigInteger(size + "")).toString.toInt)
  }
 
  private def splitArray[A](array: Array[A], parts: Int): List[Array[A]] = {
    @annotation.tailrec
    def go(parts: List[Int], array: Array[A], acc: List[Array[A]]): List[Array[A]] = parts match {
      case Nil => acc
      case (x::xs) => {
        val r = array.slice(0,x)
        go(parts.drop(1), array.drop(x), r::acc)
      }
    }
   
    go(partsPerSubArray, array, Nil).reverse
  }    
  
  
  private def md5(s: String): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(s.getBytes)



}

object BloomFilter {

  def apply(size: Int, nrHashes: Int): BloomFilter = new BloomFilter(size,nrHashes)

}

object Main {
  
  def wordGenerator(): String = {
    val r = scala.util.Random
    r.nextString(5)
  } 
  
 def main(args: Array[String]): Unit = {
    val bloomFilter = BloomFilter(8000000, 5);

    KataUtil.getFile("/wordlist.txt") { s =>
      s.getLines().foreach(bloomFilter.addKey(_))
    }
    

    println(bloomFilter.ratioPrecentage)
    val falsePositives = (1 to 1000000)
      .map(i => wordGenerator)
      .map(bloomFilter.hasKey)
      .map(a => if(a) 1 else 0)
      .reduceLeft((a,b) => a + b)
      
    println(falsePositives)
  } 
}