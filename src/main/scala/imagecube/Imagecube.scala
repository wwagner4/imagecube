package imagecube

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Pixel(x: Int, y: Int, col: Int)

case class Image(w: Int, h: Int, pixels: Seq[Int])

object Imagecube {

  def pixel(img: Image): Seq[Pixel] = {
    def rowIndexes(w: Int) = Stream.from(0).flatMap(i => List.fill(w)(i))

    def colIndexes(w: Int) = Stream.from(0).flatMap(_ => 0 until w)

    val cols = img.pixels
    cols.zip(colIndexes(img.w)).zip(rowIndexes(img.w)).map {
      case ((col, x), y) => Pixel(x, y, col)
    }
  }

  def transpose(img: Image): Image = {
    println("transposing image ...")
    val _p = img.pixels.grouped(img.w).toSeq.transpose.flatten
    Image(img.h, img.w, _p)
  }

  def rows(img: Image): Seq[Seq[Int]] = {
    img.pixels.grouped(img.w).toSeq
  }

  def cropSquare(img: Image): Image = {

    def cropSquareLands: Image = {
      val off1 = (img.w - img.h) / 2
      val off2 = img.w - off1
      val _rows = rows(img)
      val indexed = _rows.map { r => r.zipWithIndex }
      val filtered = indexed.flatMap { r =>
        r
          .filter { case (_, i) => i >= off1 && i < off2 }
          .map { case (p, _) => p }
      }
      Image(off2 - off1, img.h, filtered)
    }

    if (img.w == img.h) img
    else if (img.w > img.h) cropSquareLands
    else transpose(cropSquare(transpose(img)))
  }

  def writeImage(img: Image, file: File): Unit = {
    val bi = new BufferedImage(img.w, img.h, BufferedImage.TYPE_INT_RGB)
    pixel(img).foreach { p =>
      bi.setRGB(p.x, p.y, p.col)
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
      bi.getRGB(i, j)
    }
    Image(w, h, pxs)
  }

}
