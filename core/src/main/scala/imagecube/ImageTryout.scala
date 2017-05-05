package imagecube

import javax.imageio._
import java.io._
import java.awt.image._

object ImageTryout extends App {
  
  import Imagecube._
  
  val fNam = "core/src/test/resources/cow.jpg"
  val f = new File(fNam)
  val in = new FileInputStream(f)
  
  val bi = readImage(in, "image/jpeg")
  in.close
  
  
  val bytes = writeImage(bi, "image/png")
  
  println(bytes.take(100).mkString(","))
  
  println("F I N I S H E D")
  

}                                                              