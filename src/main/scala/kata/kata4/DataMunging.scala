package kata.kata4

import scala.io.Source
import java.io.InputStream

object DataMunging {
  
  type Label = String
  type Diff = Float
  
  def columns(line: String): Array[String] = 
    line.trim.split(" ").filter(_ != "")
    
  def getLines(fileName: String): Iterator[String] = {
    val stream : InputStream = getClass.getResourceAsStream(fileName)
    Source.fromInputStream(stream).getLines()
  }    
  
  def temperatureDiff(columns: Array[String] ): (Label,Diff) = {
    val max = columns(1).replace("*", "").toFloat
    val min = columns(2).replace("*", "").toFloat
    (columns(0),max-min)
  }
  
  def minTemperatureDiff(lines : Iterator[String]): (Label,Diff) = {
    lines.map(columns).map(temperatureDiff).reduceLeft((a,b) =>if(a._2 < b._2) a else b)
  }
  
  def goalDiff(columns: Array[String] ): (Label,Diff) = {
    val max = columns(6).toFloat
    val min = columns(8).toFloat
    (columns(1),max-min)
  }
  
  def minGoalDiff(lines: Iterator[String]): (Label,Diff) = {
    lines.map(columns).filter(_.length > 8).map(goalDiff).reduceLeft((a,b) =>if(Math.abs(a._2) < Math.abs(b._2)) a else b)
  }
  
  def main(args: Array[String]): Unit = { 
    println(minTemperatureDiff(getLines("/weather.dat").drop(2)))
    println(minGoalDiff(getLines("/football.dat").drop(1)))
  }
  
}