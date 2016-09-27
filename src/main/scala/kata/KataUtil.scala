package kata

import scala.io.Source
import java.io.InputStream

object KataUtil {
  
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

}