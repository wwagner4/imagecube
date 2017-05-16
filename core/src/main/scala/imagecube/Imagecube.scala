package imagecube

import java.awt._
import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO


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

case class Pos(x: Int, y: Int) {
  def add(other: Pos): Pos = Pos(x + other.x, y + other.y)
}

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

sealed trait HANDED

case object HANDED_Right extends HANDED

case object HANDED_Left extends HANDED


sealed trait PARTPOS

case object PARTPOS_Center extends PARTPOS

case object PARTPOS_Top extends PARTPOS

case object PARTPOS_Bottom extends PARTPOS

case object PARTPOS_Left extends PARTPOS

case object PARTPOS_Right extends PARTPOS


object Imagecube {

  def transformImageWeb(in: InputStream, inMime: String, outMime: String, maxSize: Int): Array[Byte] = {
    val bi = readImageFromStream(in, inMime)
    if (bi.getWidth() * bi.getHeight() > maxSize * 1000) {
      throw new IllegalStateException(s"Your image exceeded the maximum size of $maxSize k pixel")
    }
    in.close()
    val bo = transformImage(bi, HANDED_Right, cutLines = false)
    writeImageToByteArray(bo, outMime)
  }

  def transformImage(bi: BufferedImage, handed: HANDED, cutLines: Boolean): BufferedImage = {
    val border = 30

    val w = bi.getWidth()
    val h = bi.getHeight()
    val p: CutParams = cutParams(w, h)
    val (xrLeft, yrLeft) = transposeParamsLeft(p)
    val (xrRight, yrRight) = transposeParamsRight(p)
    val partLen = p.partLen
    println(s"reading image $w $h partlen:$partLen")

    val size = imageSize(partLen, percent(partLen, border))
    val bo = new BufferedImage(size.w, size.h, BufferedImage.TYPE_INT_RGB)
    val pos: PartsPositions = partPositions(partLen, percent(partLen, border))

    def readImage(bimg: BufferedImage, x: Range, y: Range): Seq[Seq[Int]] = {

      def readRow(j: Int, r: Range): Seq[Int] = {
        for (i <- r.from to r.to) yield {
          bimg.getRGB(i, j)
        }
      }

      for (j <- y.from to y.to) yield {
        readRow(j, x)
      }
    }

    def readImageTransp(bimg: BufferedImage, x: Range, y: Range): Seq[Seq[Int]] = {

      def readRow(j: Int, r: Range): Seq[Int] = {
        for (i <- r.to to r.from) yield {
          bimg.getRGB(j, i)
        }
      }

      for (j <- y.from to y.to) yield {
        readRow(j, x)
      }
    }

    def writeAuxLineHead(imgPart: Seq[Seq[Int]], bimg: BufferedImage, posi: Pos): Unit = {
      val auxLine = imgPart.map(l => l(0)).zipWithIndex
      (0 until percent(partLen, 7)).foreach { j =>
        auxLine.foreach {
          case (col, i) => bimg.setRGB(posi.x - j, posi.y + i, col)
        }
      }
    }

    def writeAuxLineTail(imgPart: Seq[Seq[Int]], bimg: BufferedImage, posi: Pos): Unit = {
      val li = imgPart(0).size - 1
      val auxLine = imgPart.map(l => l(li)).zipWithIndex
      (0 until percent(partLen, 7)).foreach { j =>
        auxLine.foreach {
          case (col, i) => bimg.setRGB(posi.x + j, posi.y + i, col)
        }
      }
    }

    def writeAuxLineHeadTransp(imgPart: Seq[Seq[Int]], bimg: BufferedImage, posi: Pos): Unit = {
      val auxLine = imgPart.map(l => l(0)).zipWithIndex
      (0 until percent(partLen, 7)).foreach { j =>
        auxLine.foreach {
          case (col, i) => bimg.setRGB(posi.x - j, posi.y + i, col)
        }
      }
    }

    def writeAuxLineTailTransp(imgPart: Seq[Seq[Int]], bimg: BufferedImage, posi: Pos): Unit = {
      val auxLine = imgPart.map(l => l(0)).zipWithIndex
      (0 until percent(partLen, 7)).foreach { j =>
        auxLine.foreach {
          case (col, i) => bimg.setRGB(posi.x - j, posi.y + i, col)
        }
      }
    }

    def writeImagePart(partpos: PARTPOS, bimg: BufferedImage, imgPart: Seq[Seq[Int]], posi: Pos): Unit = {
      println(s" >> writeImagePart $partpos")
      imgPart.zipWithIndex.foreach {
        case (row, i) => row.zipWithIndex.foreach {
          case (col, j) =>
            bimg.setRGB(posi.x + j, posi.y + i, col)
        }
      }
      partpos match {
        case PARTPOS_Top => writeAuxLineTail(imgPart, bimg, posi.add(Pos(partLen, 0)))
        case PARTPOS_Bottom => writeAuxLineHead(imgPart, bimg, posi)
        case _ => // Nothing to do
      }

      println(s" << writeImagePart $partpos")
    }

    def writeImagePartTransp(partpos: PARTPOS, bimg: BufferedImage, imgPart: Seq[Seq[Int]], posi: Pos): Unit = {
      println(s" >> writeImagePart transp $partpos")
      imgPart.zipWithIndex.foreach {
        case (row, i) => row.zipWithIndex.foreach {
          case (col, j) => bimg.setRGB(posi.x + i, posi.y + j, col)
        }
      }
      partpos match {
        case PARTPOS_Right => writeAuxLineTailTransp(imgPart, bimg, posi)
        case PARTPOS_Left => writeAuxLineHeadTransp(imgPart, bimg, posi)
        case _ => // Nothing to do
      }
      println(s" << writeImagePart transp $partpos")
    }

    def writeLines(g: Graphics2D, l: Int, b: Int, d: Int): Unit = {

      case class P(x: Int, y: Int) {
        def add(p: P) = P(x + p.x, y + p.y)
      }

      def drawPoli(poli: Seq[P]): Unit = {
        poli.zip(poli.tail).foreach { case (f, t) =>
          g.drawLine(f.x, f.y, t.x, t.y)
        }
      }

      def drawBack(): Unit = {
        val p = Seq(P(0, 0), P(0, l - 1), P(l - 1, l - 1), P(l - 1, 0))
          .map(_.add(P(b, b)))
          .map(_.add(P(l, 3 * l)))
        drawPoli(p)
      }

      def drawCutLines(): Unit = {
        val p = Seq(P(l, 0), P(2 * l, 0), P(2 * l, l), P(3 * l, l),
          P(3 * l, 2 * l), P(2 * l, 2 * l), P(2 * l, 3 * l), P(l, 3 * l),
          P(l, 2 * l), P(0, 2 * l), P(0, l), P(l, l), P(l, 0))
          .map(_.add(P(b, b)))
        drawPoli(p)
      }


      def drawFlapVertRight(off: P): Unit = {
        val p = Seq(P(0, 0), P(d, d), P(d, l - d), P(0, l))
          .map(_.add(P(b, b)))
          .map(_.add(off))
        drawPoli(p)
      }

      def drawFlapVertLeft(off: P): Unit = {
        val p = Seq(P(0, 0), P(-d, d), P(-d, l - d), P(0, l))
          .map(_.add(P(b, b)))
          .map(_.add(off))
        drawPoli(p)
      }

      def drawFlapHorDown(off: P): Unit = {
        val p = Seq(P(0, 0), P(d, d), P(l - d, d), P(l, 0))
          .map(_.add(P(b, b)))
          .map(_.add(off))
        drawPoli(p)
      }

      def drawFlapHorUp(off: P): Unit = {
        val p = Seq(P(0, 0), P(d, -d), P(l - d, -d), P(l, 0))
          .map(_.add(P(b, b)))
          .map(_.add(off))
        drawPoli(p)
      }

      println("write lines")

      g.setColor(Color.BLACK)
      drawBack()

      if (cutLines) {
        g.setColor(new Color(237, 237, 237))
        drawCutLines()
      }

      g.setColor(Color.BLACK)
      handed match {
        case HANDED_Right =>
          drawFlapVertRight(P(2 * l, 0))
          drawFlapHorUp(P(0, l))
          drawFlapVertLeft(P(l, 2 * l))
          drawFlapHorDown(P(2 * l, 2 * l))
        case HANDED_Left =>
          drawFlapVertLeft(P(l, 0))
          drawFlapHorDown(P(0, 2 * l))
          drawFlapVertRight(P(2 * l, 2 * l))
          drawFlapHorUp(P(2 * l, l))
      }


      drawFlapVertLeft(P(l, 3 * l))
      drawFlapHorDown(P(l, 4 * l))
      drawFlapVertRight(P(2 * l, 3 * l))
    }


    def processCenter(bo: BufferedImage, bimg: BufferedImage, rx: Range, ry: Range): Unit = {
      val img = readImage(bimg, rx, ry)
      writeImagePart(PARTPOS_Center, bo, img, pos.center)
    }

    def processLeft(bo: BufferedImage, bimg: BufferedImage, rx: Range, ry: Range): Unit = {
      val img = readImageTransp(bimg, rx, ry)
      val imgs = shortenImagePartSeq(PARTPOS_Left, img)
      writeImagePartTransp(PARTPOS_Left, bo, imgs, pos.left)
    }

    def processRight(bo: BufferedImage, bimg: BufferedImage, rx: Range, ry: Range): Unit = {
      val img = readImageTransp(bimg, rx, ry)
      val imgs = shortenImagePartSeq(PARTPOS_Right, img.reverse).reverse
      writeImagePartTransp(PARTPOS_Right, bo, imgs, pos.right)
    }

    def processTop(bo: BufferedImage, bimg: BufferedImage, rx: Range, ry: Range): Unit = {
      val img = readImage(bimg, rx, ry)
      val imgs = shortenImagePartSeq(PARTPOS_Top, img)
      writeImagePart(PARTPOS_Top, bo, imgs, pos.top)
    }

    def processBottom(bo: BufferedImage, bimg: BufferedImage, rx: Range, ry: Range): Unit = {
      val img = readImage(bimg, rx, ry)
      val imgs = shortenImagePartSeq(PARTPOS_Bottom, img.reverse).reverse
      writeImagePart(PARTPOS_Bottom, bo, imgs, pos.bottom)
    }

    def processParSeq(): Unit = {
      Seq(
        () => processCenter(bo, bi, p.x2, p.y2),
        () => processLeft(bo, bi, xrLeft, yrLeft),
        () => processRight(bo, bi, xrRight, yrRight),
        () => processTop(bo, bi, Range(p.x1.from, p.x3.to), p.y1),
        () => processBottom(bo, bi, Range(p.x1.from, p.x3.to), p.y3)
      ).par.foreach(f => f())
    }

    val g = bo.getGraphics.asInstanceOf[Graphics2D]

    g.setColor(Color.WHITE)
    g.fillRect(0, 0, bo.getWidth, bo.getHeight())

    processParSeq()

    writeLines(g, partLen, percent(partLen, border), percent(partLen, 15))

    bo
  }


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

  def shortenImagePartSeq(partpos: PARTPOS, part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val newRowsA = shortenRowsA(partpos, part)
    val newRowsB = shortenRowsB(partpos, part)
    newRowsA.zip(newRowsB).map { case (a, b) => a ++ b }
  }

  def shortenRowsA(partpos: PARTPOS, part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    println(s" >> shorten $partpos A")
    val re = part.zipWithIndex.map { case (row, i) =>
      val n = part.size
      val (from, to) = shortenA(i, n)
      val filteredCol = row.zipWithIndex
        .filter { case (_, ir) => ir >= from && ir <= to }
        .map { case (c, _) => c }
      linearCompress(filteredCol, n / 2, colorMix)
    }
    println(s" << shorten $partpos A")
    re
  }

  def shortenRowsB(partpos: PARTPOS, part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    println(s" >> shorten $partpos B")
    val re = part.zipWithIndex.map { case (row, i) =>
      val n = part.size
      val (from, to) = shortenB(i, n)
      val filteredCol = row.zipWithIndex
        .filter { case (_, ir) => ir >= from && ir <= to }
        .map { case (c, _) => c }
      val m = if (n % 2 == 0) n / 2 else n / 2 + 1
      linearCompress(filteredCol, m, colorMix)
    }
    println(s" << shorten $partpos B")
    re
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

  def imageType(file: File): String = {
    val i = file.getName.lastIndexOf('.')
    if (i < 0) throw new IllegalArgumentException("file must have an image extension. .png, .jpg, ...")
    val ext = file.getName.substring(i + 1).toUpperCase
    ext match {
      case "JPG" => "JPEG"
      case "PNG" => ext
      case "BMP" => ext
      case "WBMP" => ext
      case "JPEG" => ext
      case _ => throw new IllegalStateException(s"unknown image format $ext")
    }
  }


  def writeImageToByteArray(bi: BufferedImage, mimeType: String): Array[Byte] = {
    import scala.collection.JavaConverters._

    val os = new ByteArrayOutputStream()
    val writers = ImageIO.getImageWritersByMIMEType(mimeType).asScala.toList
    val wrt = if (writers.nonEmpty) writers.head
    else throw new IllegalStateException(s"Unknown image mime type'$mimeType'")
    val ios = ImageIO.createImageOutputStream(os)
    wrt.setOutput(ios)
    wrt.write(bi)
    os.close()
    os.toByteArray
  }

  def readImageFromStream(in: InputStream, mimeType: String): BufferedImage = {
    import scala.collection.JavaConverters._

    val readers = ImageIO.getImageReadersByMIMEType(mimeType).asScala.toList
    val reader = if (readers.nonEmpty) readers.head
    else throw new IllegalStateException(s"Unknown image mime type'$mimeType'")
    val iis = ImageIO.createImageInputStream(in)
    reader.setInput(iis)
    reader.read(reader.getMinIndex)
  }


  def percent(value: Int, perc: Int): Int = (value.toDouble * perc / 100.0).round.toInt

  def extractName(f: File): String = {
    val i = f.getName.lastIndexOf('.')
    if (i < 0) f.getName
    else f.getName.substring(0, i)
  }


}
