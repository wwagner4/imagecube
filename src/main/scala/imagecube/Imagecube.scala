package imagecube

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Col(r: Int, g: Int, b: Int)

case class Pixel(x: Int, y: Int, col: Col)

case class Image(w: Int, h: Int, pixels: Seq[Col])

object Imagecube {

  def mix(p1: Col, p2: Col): Col = {
    Col(
      (p1.r + p2.r) / 2,
      (p1.g + p2.g) / 2,
      (p1.b + p2.b) / 2
    )
  }


  def pixel(img: Image): Seq[Pixel] = {
    def rowIndexes(w: Int): Stream[Int] = Stream.from(0).flatMap(i => List.fill(w)(i))
    def colIndexes(w: Int): Stream[Int] = Stream.from(0).flatMap(_ => 0 until w)
    val cols = img.pixels
    cols.zip(colIndexes(img.w)).zip(rowIndexes(img.w)).map {
      case ((col, x), y) => Pixel(x, y, col)
    }
  }

  def rows(img: Image): Seq[Seq[Col]] = {
    img.pixels.grouped(img.w).toSeq
  }

  def cols(img: Image): Seq[Seq[Col]] = {
    img.pixels.grouped(img.w).toSeq.transpose
  }

  def cropSquare(img: Image): Image = {

    def cropSquare1: Image = {
      val off = (img.w - img.h) / 2
      ???

    }

    def cropSquare2: Image = {
      ???
    }

    if (img.w == img.h) img
    else if (img.w > img.h) cropSquare1
    else cropSquare2
  }

  def writeImage(img: Image, file: File): Unit = {
    val bi = new BufferedImage(img.w, img.h, BufferedImage.TYPE_INT_RGB)
    pixel(img).foreach { p =>
      val c = new Color(p.col.r, p.col.g, p.col.b)
      bi.setRGB(p.x, p.y, c.getRGB)
    }
    ImageIO.write(bi, imageType(file), file)
  }

  def imageType(file: File): String = {
    val i = file.getName.lastIndexOf('.')
    if (i < 0) throw new IllegalArgumentException("file must have an image extension. .png, .jpg, ...")
    val ext = file.getName.substring(i + 1).toUpperCase
    ext match {
      case "JPEG" => "JPG"
      case _ => ext
    }
  }

  def readImage(file: File): Image = {
    val bi = ImageIO.read(file)
    val w = bi.getWidth()
    val h = bi.getHeight()
    val pxs = for (j <- 0 until h; i <- 0 until w) yield {
      val rgb = bi.getRGB(i, j)
      val c = new Color(rgb)
      Col(c.getRed, c.getGreen, c.getBlue)
    }
    Image(w, h, pxs)
  }


}
