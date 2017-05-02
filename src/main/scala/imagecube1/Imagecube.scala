package imagecube1

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Img(
                partLen: Int,
                center: Seq[Seq[Int]],
                left: Seq[Seq[Int]],
                right: Seq[Seq[Int]],
                top: Seq[Seq[Int]],
                bottom: Seq[Seq[Int]]
              )

case class Range(
                  from: Int,
                  to: Int
                )

case class CutParams(
                      partLen: Int,
                      x1: Range,
                      x2: Range,
                      x3: Range,
                      y1: Range,
                      y2: Range,
                      y3: Range
                    )

case class Pos(
                x: Int,
                y: Int
              )

case class PartsPositions(
                           center: Pos,
                           left: Pos,
                           right: Pos,
                           top: Pos,
                           bottom: Pos
                         )

case class Size(
                 w: Int,
                 h: Int
               )

object Imagecube {

  def imageSize(partLen: Int, border: Int): Size = {
    Size(
      2 * border + 3 * partLen,
      2 * border + 4 * partLen
    )
  }

  def partPositions(partLen: Int, border: Int): PartsPositions = {
    val center = Pos(border + partLen, border + partLen)
    val left = Pos(border, border + partLen)
    val right = Pos(border + 2 * partLen, border + partLen)
    val top = Pos(border + partLen, border)
    val bottom = Pos(border + partLen, border + 2 * partLen)
    PartsPositions(center, left, right, top, bottom)
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
    val p = cutParams(w, h)
    println(s"readImage $p")
    val (xrLeft, yrLeft) = transposeParamsLeft(p)
    val (xrRight, yrRight) = transposeParamsRight(p)
    Img(
      partLen = p.partLen,
      center = readImage(bi, p.x2, p.y2),
      left = readImageTransp(bi, xrLeft, yrLeft),
      right = readImageTransp(bi, xrRight, yrRight),
      top = readImage(bi, Range(p.x1.from, p.x3.to), p.y1),
      bottom = readImage(bi, Range(p.x1.from, p.x3.to), p.y3)
    )
  }

  def shortenImg(img: Img): Img = {
    Img(
      img.partLen,
      img.center,
      shortenImagePart(img.left),
      shortenImagePart(img.right),
      shortenImagePart(img.top),
      shortenImagePart(img.bottom)
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

  def transposeParamsLeft(p: CutParams): (Range, Range) = {
    val x = Range(p.y3.to, p.y1.from)
    val y = Range(p.x1.from, p.x1.to)
    (x, y)
  }

  def transposeParamsRight(p: CutParams): (Range, Range) = {
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
        step,
        Range(off1, off1 + step - 1),
        Range(off1 + step, off1 + (2 * step) - 1),
        Range(off1 + (2 * step), off1 + (3 * step) - 1),
        Range(0, step - 1),
        Range(step, (2 * step) - 1),
        Range(2 * step, (3 * step) - 1)
      )
    }

    def transpose(p: CutParams): CutParams = {
      CutParams(p.partLen, p.y1, p.y2, p.y3, p.x1, p.x2, p.x3)
    }

    if (width >= height) cutParamsPort(width, height)
    else transpose(cutParams(height, width))
  }

  def shortenImagePart(part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
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
    newRowsA.zip(newRowsB).map { case (a, b) => a ++ b }
  }

  def shortenA(i: Int, n: Int): (Int, Int) = {
    val a = (3.0 * n / 2.0).round.toInt
    val from = i
    val to = a
    (from, to)
  }

  def shortenB(i: Int, n: Int): (Int, Int) = {
    val a = (3.0 * n / 2.0).round.toInt
    val from = a
    val to = 2 * a - i
    (from, to)
  }

}
