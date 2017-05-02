package imagecube1

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Img(
                center: Seq[Seq[Int]],
                left: Seq[Seq[Int]],
                right: Seq[Seq[Int]],
                top: Seq[Seq[Int]],
                bottom: Seq[Seq[Int]]
              )

object Imagecube {
  
  import ImagecubeUtil._
  
   def shortenImagePart(part: Seq[Seq[Int]]):  Seq[Seq[Int]] = {
      val newRowsA = part.zipWithIndex.map { case (row, i) =>
        val n = part.size
        val (from, to) = shortenA(i, n)
        val filteredCol = row.zipWithIndex
          .filter { case (_, ir) => ir >= from && ir <= to }
          .map { case (c, _) => c }
        linearCompress(filteredCol, n / 2, colorMix)
      }
      val newRowsB = part.zipWithIndex.map { case (row, i) =>
        val n = part.size
        val (from, to) = shortenB(i, n)
        val filteredCol = row.zipWithIndex
          .filter { case (_, ir) => ir >= from && ir <= to }
          .map { case (c, _) => c }
        val m = if (n % 2 == 0) n / 2 else n / 2 + 1
        linearCompress(filteredCol, m, colorMix)
      }
      newRowsA.zip(newRowsB).map { case (a, b) => a ++ b}
    }

  def readImage(file: File): Img = {

    def readImage(bi: BufferedImage, x: Range, y: Range): Seq[Seq[Int]] = {
      println(s"readImage $x $y")

      def readRow(j: Int, r: Range): Seq[Int] = {
        for (i <- r.from to r.to) yield {
          bi.getRGB(i, j)
        }
      }

      for (j <- y.from to y.to) yield {
        readRow(j, x)
      }
    }


    def readImageTransp(bi: BufferedImage, x: Range, y: Range): Seq[Seq[Int]] = {
      println(s"readImageTransp $x $y")

      def readRow(j: Int, r: Range): Seq[Int] = {
        for (i <- r.to to r.from) yield {
          bi.getRGB(j, i)
        }
      }

      for (j <- y.from to y.to) yield {
        readRow(j, x)
      }
    }

    val bi = ImageIO.read(file)
    val w = bi.getWidth()
    val h = bi.getHeight()
    println(s"readImage $w $h")
    val p = ImagecubeUtil.cutParams(w, h)
    println(s"readImage $p")
    val (xrLeft, yrLeft) = ImagecubeUtil.transposeParamsLeft(p)
    val (xrRight, yrRight) = ImagecubeUtil.transposeParamsRight(p)
    Img(
      center = readImage(bi, p.x2, p.y2),
      left = readImageTransp(bi, xrLeft, yrLeft),
      right = readImageTransp(bi, xrRight, yrRight),
      top = readImage(bi, Range(p.x1.from, p.x3.to), p.y1),
      bottom = readImage(bi, Range(p.x1.from, p.x3.to), p.y3)
    )
  }

  def colorMix(colors: Seq[Int]): Int = {
    case class Rgb(r: Int, g: Int, b: Int)
    def rgb(col: Int): Rgb = {
      Rgb((col & 0xff0000) >> 16, (col & 0xff00) >> 8, col & 0xff)
    }

    val size = colors.size
    val rgbs = colors.map(rgb)
    val sum = rgbs.foldLeft(Rgb(0, 0, 0))((a, b) => Rgb(a.r + b.r, a.g + b.g, a.b + b.b))
    val mean = Rgb(sum.r / size, sum.g / size, sum.b / size)
    255 << 24 | mean.r << 16 | mean.g << 8 | mean.b
  }

  def linearCompress[T](in: Seq[T], n: Int, f: Seq[T] => T): Seq[T] = {

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

}
