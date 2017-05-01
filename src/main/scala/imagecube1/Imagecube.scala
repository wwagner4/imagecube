package imagecube1

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Range(from: Int, to: Int)

case class CutParams(x1: Range, x2: Range, x3: Range, y1: Range, y2: Range, y3: Range)

case class Pix(i: Int, col: Int)

case class Row(i: Int, pixs: Seq[Pix])


case class Img(
                center: Seq[Row],
                left: Seq[Row],
                right: Seq[Row],
                top: Seq[Row],
                bottom: Seq[Row]
              )

object Imagecube {

  def readImage(file: File): Img = {

    def readImage(bi: BufferedImage, x: Range, y: Range): Seq[Row] = {
      println(s"readImage $x $y")

      def readRow(j: Int, r: Range): Seq[Pix] = {
        for (i <- r.from to r.to) yield {
          val c = bi.getRGB(i, j)
          Pix(i, c)
        }
      }

      for (j <- y.from to y.to) yield {
        Row(j, readRow(j, x))
      }
    }

    def readImageTransp(bi: BufferedImage, x: Range, y: Range): Seq[Row] = {
      println(s"readImageTransp $x $y")

      def readRow(j: Int, r: Range): Seq[Pix] = {
        for (i <- r.to to(r.from, -1)) yield {
          val c = bi.getRGB(j, i)
          Pix(i, c)
        }
      }

      for (j <- x.to to(x.from, -1)) yield {
        Row(j, readRow(j, y))
      }
    }

    val bi = ImageIO.read(file)
    val w = bi.getWidth()
    val h = bi.getHeight()
    println(s"readImage $w $h")
    val p = cutParams(w, h)
    println(s"readImage $p")
    val (xrLeft, yrLeft) = transposeParamsLeft(w, h, p)
    val (xrRight, yrRight) = transposeParamsRight(w, h, p)
    Img(
      center = readImage(bi, p.x2, p.y2),
      left = readImageTransp(bi, xrLeft, yrLeft),
      right = readImageTransp(bi, xrRight, yrRight),
      top = readImage(bi, Range(p.x1.from, p.x3.to), p.y1),
      bottom = readImage(bi, Range(p.x1.from, p.x3.to), p.y3)
    )
  }

  def linCompress[T](in: Seq[T], n: Int, f: Seq[T] => T): Seq[T] = {

    def diff(s: Stream[Int]): Stream[Int] = {
      s.zip(s.tail).map { case (a, b) => b - a }
    }

    def process(in: Seq[T], diffs: Stream[Int]): Seq[T] = {
      if (in == Nil) Nil
      else {
        val diff = diffs.head
        val (elems, rest) = in.splitAt(diff)
        f(elems) +: process(rest, diffs.tail)
      }
    }

    require(in.size >= n, s"size of 'in' (${in.size}) must be greater than 'n' ($n)")
    if (in.size == n) in
    else {
      val v = in.size.toDouble / n
      val borders = Stream.iterate(0.0)(x => x + v).map(x => x.round.toInt)
      val diffs = diff(borders)
      process(in, diffs)
    }
  }




  def transposeParamsLeft(w: Int, h: Int, p: CutParams): (Range, Range) = {
    val x = Range(p.y3.to, p.y1.from)
    val y = Range(p.x1.from, p.x1.to)
    (x, y)
  }

  def transposeParamsRight(w: Int, h: Int, p: CutParams): (Range, Range) = {
    val x = Range(p.y3.to, p.y1.from)
    val y = Range(p.x3.from, p.x3.to)
    (x, y)
  }

  def cutParams(width: Int, height: Int): CutParams = {

    def cutParamsPort(w: Int, h: Int): CutParams = {
      val h1 = (h / 3) * 3
      val off1 = (w - h1) / 2
      val step = h1 / 3
      CutParams(
        Range(off1, off1 + step - 1),
        Range(off1 + step, off1 + (2 * step) - 1),
        Range(off1 + (2 * step), off1 + (3 * step) - 1),
        Range(0, step - 1),
        Range(step, (2 * step) - 1),
        Range(2 * step, (3 * step) - 1)
      )
    }

    def transpose(p: CutParams): CutParams = {
      CutParams(
        p.y1,
        p.y2,
        p.y3,
        p.x1,
        p.x2,
        p.x3
      )
    }

    if (width >= height) cutParamsPort(width, height)
    else transpose(cutParams(height, width))
  }

}
