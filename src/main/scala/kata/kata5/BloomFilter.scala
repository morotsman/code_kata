package kata.kata5

import java.security.MessageDigest
import java.math.BigInteger
import scala.io.Source
import scala.io.StdIn
import java.io.InputStream

class BloomFilter(size: Int, nrHashes: Int) {
  
  if(nrHashes > 4) {
    throw new RuntimeException("To many hashes, at most 4 are permitted")
  }

  val bitmap: Array[Boolean] = new Array(size)

  private def md5(s: String): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(s.getBytes)

  private def splitArray(a: Array[Byte], nrOfParts: Int): Iterator[Array[Byte]] = 
    a.grouped(a.size / nrOfParts).filter(_.length == a.size / nrOfParts)

  private def createHashes(s: String): Iterator[Int] = {
    splitArray(md5(s), nrHashes).map(a => (new BigInteger(a)).mod(new BigInteger(size + "")).toString.toInt)
  }

  def addKey(s: String): Unit =
    createHashes(s).foreach(bitmap(_) = true)

  def hasKey(s: String): Boolean =
    createHashes(s).forall(bitmap(_) == true)

  def ratioPrecentage(): Float =
    (bitmap.filter(_ == true).size).toFloat / size.toFloat * 100

}

object BloomFilter {

  def wordGenerator(): String = {
    val r = scala.util.Random
    r.nextString(5)
  }

  def getFile[A](fileName: String)(f: Source => A): A = {
    var stream: Option[InputStream] = None
    var source: Option[Source] = None
    try {
      stream = Some(getClass.getResourceAsStream(fileName))
      source = Some(Source.fromInputStream(stream.get, "utf-8"))
      f(source.get)
    } finally {
      source.foreach(_.close)
      stream.foreach(_.close)
    }
  }

  def main(args: Array[String]): Unit = {
    val bloomFilter = new BloomFilter(8000000, 4);

    getFile("/wordlist.txt") { s =>
      s.getLines().foreach(bloomFilter.addKey(_))
    }
    

    println(bloomFilter.ratioPrecentage)
    val falsePositives = (1 to 100000)
      .map(i => wordGenerator)
      .map(bloomFilter.hasKey)
      .map(a => if(a) 1 else 0)
      .reduceLeft((a,b) => a + b)
      
    println(falsePositives)


  }

}