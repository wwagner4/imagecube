package imagecube

import java.awt.Color
import java.io.File
import java.util.UUID


object Tryout extends App {

  // convertColorToPixel()
  // readImageToBufferedImage()
  // exploreImage()
  // copyImage()
  cropImage()
  // intArithm()

  def intArithm(): Unit = {
    val w = 7
    val h = 3
    val off1 = (w - h)  / 2
    val off2 = w - off1
    println(off1)
    println(off2)
  }

  def dir: File = new File("src/main/resources")
  def tmpdir: File = {
    val re = new File("target/tmp")
    if (!re.exists()) re.mkdirs()
    re
  }

  def copyImage(): Unit ={
    import imagecube.Imagecube._
    val img = readImage(new File(dir, "cow.jpg"))
    val outName = s"out${UUID.randomUUID()}.jpg"
    val outFile = new File(tmpdir, outName)
    writeImage(img, outFile)
    println(s"wrote to $outFile")
  }

  def cropImage(): Unit ={
    import imagecube.Imagecube._
    val img = readImage(new File(dir, "cow.jpg"))
    val crop = cropSquare(img)
    val outName = s"out${UUID.randomUUID()}.jpg"
    val outFile = new File(tmpdir, outName)
    writeImage(crop, outFile)
    println(s"wrote to $outFile")
  }

  def readImageToBufferedImage(): Unit = {
    import Imagecube._
    val img = readImage(new File(dir, "tiny.jpg"))
    println(img)
  }

  def exploreImage(): Unit = {
    import Imagecube._
    val img = readImage(new File(dir, "tiny.jpg"))
    println(img)
  }

  def convertColorToPixel(): Unit = {
    val color = new Color(255, 20, 30)

    println(color.getRed.getClass)
    println(color.getGreen)
    println(color.getBlue)
  }


}
