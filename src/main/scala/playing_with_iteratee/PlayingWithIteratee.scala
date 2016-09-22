package playing_with_iteratee

import play.api.libs.iteratee._
import scala.concurrent._
import scala.concurrent.duration._
import akka.pattern.after
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

import java.util.Date

object PlayingWithIteratee {
  
  
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("HelloSystem")
    val timeStream = Enumerator.generateM {
      after(1000.milliseconds, system.scheduler)(Future(Some(new Date)))
    }
    
    val printlnSink = Iteratee.foreach[Date](date => println(date))
    
    timeStream |>> printlnSink
    

    
  }

  
}