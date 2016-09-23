package kata.kata4

import scala.io.Source
import java.io.InputStream

object DataMunging {

  type Label = String
  type Result = Float

  def columns(line: String): Array[String] =
    line.trim.split(" ").filter(_ != "")

  def getRows(fileName: String): Iterator[Array[String]] = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    Source.fromInputStream(stream).getLines().map(columns)
  }

  def columnSelector(columnIndexes: Int*): Array[String] => Array[String] =
    columns => {
      for (
        columnIndex <- columnIndexes.toArray
      ) yield columns(columnIndex)
    }

  def resultCreator(columns: Array[String]): (Label, Result) =
    (columns(0), columns(1).toFloat - columns(2).toFloat)

  def temperatureFilter(columns: Array[String]): Boolean = true

  def temperatureMapper(columns: Array[String]): (Label, Result) =
    (columnSelector(0, 1, 2)
      andThen (a => Array(a(0), a(1).replace("*", ""), a(2).replace("*", "")))
      andThen resultCreator)(columns)

  def footballFilter(columns: Array[String]): Boolean = columns.length > 8

  def footballMapper(columns: Array[String]): (Label, Result) =
    (columnSelector(1, 6, 8) andThen resultCreator)(columns)

  def reducer(a: (Label, Result), b: (Label, Result)): (Label, Result) =
    if (Math.abs(a._2) < Math.abs(b._2)) a else b

  def main(args: Array[String]): Unit = {

    val minTemperatureDiff = getRows("/weather.dat").drop(2)
      .filter(temperatureFilter)
      .map(temperatureMapper)
      .reduceLeft(reducer)
    println(minTemperatureDiff)

    val minGoalDiff = getRows("/football.dat").drop(1)
      .filter(footballFilter)
      .map(footballMapper)
      .reduceLeft(reducer)
    println(minGoalDiff)

  }

}