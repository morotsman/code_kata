package kata.kata4

import scala.io.Source
import java.io.InputStream
import Numeric._

object DataMunging {

  type Label = String

  def toColumns(line: String): Array[String] =
    line.trim.split(" ").filter(_ != "")

  def getRows(fileName: String): Iterator[Array[String]] = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    Source.fromInputStream(stream).getLines().map(toColumns)
  }

  def columnSelector(columnIndexes: Int*): Array[String] => Array[String] =
    columns => {
      for (
        columnIndex <- columnIndexes.toArray
      ) yield columns(columnIndex)
    }

  def temperatureMapper(columns: Array[String]): (Label, Float) =
    (columnSelector(0, 1, 2)
      andThen (a => Array(a(0), a(1).replace("*", ""), a(2).replace("*", "")))
      andThen (a => (a(0), a(1).toFloat - a(2).toFloat)))(columns)

  def footballFilter(columns: Array[String]): Boolean = columns.length > 8

  def footballMapper(columns: Array[String]): (Label, Int) =
    (columnSelector(1, 6, 8) 
        andThen (a => (a(0), a(1).toInt - a(2).toInt)))(columns)  
    
  def reducer[A](a: (Label, A), b: (Label, A))(implicit num: Numeric[A]): (Label, A) =
      if (num.lt(num.abs(a._2), num.abs(b._2))) a else b
       
  def main(args: Array[String]): Unit = {
        
    val minTemperatureDiff = getRows("/weather.dat").drop(2)
      .map(temperatureMapper)
      .reduceLeft(reducer[Float])
    println(minTemperatureDiff)

    val minGoalDiff = getRows("/football.dat").drop(1)
      .filter(footballFilter)
      .map(footballMapper)
      .reduceLeft(reducer[Int])
    println(minGoalDiff)

  }

}