package imagecube

import javax.imageio._
import java.io._
import java.awt.image._

object ImageTryout extends App {
  val fNam = "core/src/test/resources/cow.jpg"
  val f = new File(fNam)
  val in = new FileInputStream(f)
  
  val bi = readStream(in, "image/jpeg")
  in.close
  println("F I N I S H E D")
  
  def readStream(in: InputStream, mimeType: String): BufferedImage = {
    import scala.collection.JavaConverters._
    
    println(s"readStream $in $mimeType")
    val readers = ImageIO.getImageReadersByMIMEType(mimeType).asScala.toList
    val reader = if (!readers.isEmpty) readers(0) else throw new IllegalStateException(s"Unknown image mime type'$mimeType'")   
    println(s"reader: $reader")
  
    val iis=ImageIO.createImageInputStream(in);
    reader.setInput(iis);
    reader.read(reader.getMinIndex)
  }

}