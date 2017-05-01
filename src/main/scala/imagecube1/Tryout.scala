package imagecube1

import java.io.File


object Tryout extends App {

  readFile()
  // createCutParams()

  def dir: File = new File("src/main/resources")

  def tmpdir: File = {
    val re = new File("target/tmp")
    if (!re.exists()) re.mkdirs()
    re
  }

  def createCutParams(): Unit ={
    val whs = Seq(
      (5, 6),
      (5, 7),
      (5, 8),
      (6, 6),
      (7, 6),
      (8, 6)
    )

    whs.foreach{
      case (w, h) => println(s"($w, $h, ${Imagecube.cutParams(w, h)}, (Range(0, 0), Range(0, 0))),")
    }
  }

  def readFile(): Unit = {
    import Imagecube._
    val fName = "tiny1.jpg"
    val f = new File(dir, fName)
    val img = readImage(f).toString().take(100) + " ..."
    println(s"created img for $fName")
    println(s"img: $img")
  }

}
