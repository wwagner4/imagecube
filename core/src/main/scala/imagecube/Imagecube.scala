package imagecube

import java.awt._
import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._


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

sealed trait RUNMODE

case object RUNMODE_Seq extends RUNMODE

case class RUNMODE_Parallel(timeout: Duration) extends RUNMODE

object Imagecube {

  def transformImageWeb(in: InputStream, inMime: String, outMime: String): Array[Byte] = {
    val bi = readImageFromStream(in, inMime)
    in.close()
    val bo = transformImage(bi)
    writeImageToByteArray(bo, outMime)
  }

  def transformImage(bi: BufferedImage): BufferedImage = {
    val border = 20

    val w = bi.getWidth()
    val h = bi.getHeight()
    println(s"reading image $w $h")
    val p: CutParams = cutParams(w, h)
    val (xrLeft, yrLeft) = transposeParamsLeft(p)
    val (xrRight, yrRight) = transposeParamsRight(p)
    val partLen = p.partLen

    val size = imageSize(partLen, border)
    val bo = new BufferedImage(size.w, size.h, BufferedImage.TYPE_INT_RGB)
    val pos: PartsPositions = partPositions(partLen, border)

    def readImage(bi: BufferedImage, x: Range, y: Range): Seq[Seq[Int]] = {

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

      def readRow(j: Int, r: Range): Seq[Int] = {
        for (i <- r.to to r.from) yield {
          bi.getRGB(j, i)
        }
      }

      for (j <- y.from to y.to) yield {
        readRow(j, x)
      }
    }

    def writeImagePart(partId: String, bo: BufferedImage, imgPart: Seq[Seq[Int]], pos: Pos): Unit = {
      println(s"writeImagePart $partId")
      imgPart.zipWithIndex.foreach {
        case (row, i) => row.zipWithIndex.foreach {
          case (col, j) => bo.setRGB(pos.x + j, pos.y + i, col)
        }
      }
    }

    def writeImagePartTransp(partId: String, bo: BufferedImage, imgPart: Seq[Seq[Int]], pos: Pos): Unit = {
      println(s"writeImagePart transp $partId")
      imgPart.zipWithIndex.foreach {
        case (row, i) => row.zipWithIndex.foreach {
          case (col, j) => bo.setRGB(pos.x + i, pos.y + j, col)
        }
      }
    }

    def processCenter(bo: BufferedImage, bi: BufferedImage, rx:Range, ry: Range): Unit = {
      val img = readImage(bi, rx, ry)
      writeImagePart("C", bo, img, pos.center)
    }
    def processLeft(bo: BufferedImage, bi: BufferedImage, rx:Range, ry: Range): Unit = {
      val img = readImageTransp(bi, rx, ry)
      val imgs = shortenImagePartSeq("L", img)
      writeImagePartTransp("L", bo, imgs, pos.left)
    }
    def processRight(bo: BufferedImage, bi: BufferedImage, rx:Range, ry: Range): Unit = {
      val img = readImageTransp(bi, rx, ry)
      val imgs = shortenImagePartSeq("R", img.reverse).reverse
      writeImagePartTransp("R", bo, imgs, pos.right)
    }
    def processTop(bo: BufferedImage, bi: BufferedImage, rx:Range, ry: Range): Unit = {
      val img = readImage(bi, rx, ry)
      val imgs = shortenImagePartSeq("T", img)
      writeImagePart("T", bo, imgs, pos.top)
    }
    def processBottom(bo: BufferedImage, bi: BufferedImage, rx:Range, ry: Range): Unit = {
      val img = readImage(bi, rx, ry)
      val imgs = shortenImagePartSeq("B", img.reverse).reverse
      writeImagePart("B", bo, imgs, pos.bottom)
    }

    processCenter(bo, bi, p.x2, p.y2)
    processLeft(bo, bi, xrLeft, yrLeft)
    processRight(bo, bi, xrRight, yrRight)
    processTop(bo, bi, Range(p.x1.from, p.x3.to), p.y1)
    processBottom(bo, bi, Range(p.x1.from, p.x3.to), p.y3)

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

  def readImage(bi: BufferedImage): Img = {

    def readImage(bi: BufferedImage, x: Range, y: Range): Seq[Seq[Int]] = {

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

      def readRow(j: Int, r: Range): Seq[Int] = {
        for (i <- r.to to r.from) yield {
          bi.getRGB(j, i)
        }
      }

      for (j <- y.from to y.to) yield {
        readRow(j, x)
      }
    }

    val w = bi.getWidth()
    val h = bi.getHeight()
    println(s"reading image $w $h")
    val p: CutParams = cutParams(w, h)
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

  def shortenImgSeq(img: Img): Img = {
    Img(
      img.partLen,
      img.center,
      shortenImagePartSeq("L", img.left),
      shortenImagePartSeq("R", img.right.reverse).reverse,
      shortenImagePartSeq("T", img.top),
      shortenImagePartSeq("B", img.bottom.reverse).reverse
    )
  }

  def shortenImgPar(img: Img, timeout: Duration): Img = {
    val fLeft = Future {
      shortenImagePartPar("L", img.left, timeout)
    }
    val fRight = Future {
      shortenImagePartPar("R", img.right.reverse, timeout).reverse
    }
    val fTop = Future {
      shortenImagePartPar("T", img.top, timeout)
    }
    val fBottom = Future {
      shortenImagePartPar("B", img.bottom.reverse, timeout).reverse
    }
    val r = for (rLeft <- fLeft; rRight <- fRight; rTop <- fTop; rBottom <- fBottom)
      yield (rLeft, rRight, rTop, rBottom)
    val (left, right, top, bottom) = Await.result(r, timeout)
    Img(
      img.partLen, img.center, left, right, top, bottom
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

  def shortenImagePartSeq(partId: String, part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val newRowsA = shortenRowsA(partId, part)
    val newRowsB = shortenRowsB(partId, part)
    newRowsA.zip(newRowsB).map { case (a, b) => a ++ b }
  }

  def shortenImagePartPar(partId: String, part: Seq[Seq[Int]], timeout: Duration): Seq[Seq[Int]] = {

    println(s"shorten part $partId")

    val fRowsA = Future {
      shortenRowsA(partId, part)
    }
    val fRowsB = Future {
      shortenRowsB(partId, part)
    }
    val r = for (rRowsA <- fRowsA; rRowsB <- fRowsB) yield (rRowsA, rRowsB)
    val (newRowsA, newRowsB) = Await.result(r, timeout)
    newRowsA.zip(newRowsB).map { case (a, b) => a ++ b }
  }

  def shortenRowsA(partId: String, part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    println(s"shorten $partId A")
    part.zipWithIndex.map { case (row, i) =>
      val n = part.size
      val (from, to) = shortenA(i, n)
      val filteredCol = row.zipWithIndex
        .filter { case (_, ir) => ir >= from && ir <= to }
        .map { case (c, _) => c }
      linearCompress(filteredCol, n / 2, colorMix)
    }
  }

  def shortenRowsB(partId: String, part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    println(s"shorten $partId B")
    part.zipWithIndex.map { case (row, i) =>
      val n = part.size
      val (from, to) = shortenB(i, n)
      val filteredCol = row.zipWithIndex
        .filter { case (_, ir) => ir >= from && ir <= to }
        .map { case (c, _) => c }
      val m = if (n % 2 == 0) n / 2 else n / 2 + 1
      linearCompress(filteredCol, m, colorMix)
    }
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

  def createImage(img: Img, border: Int, runMode: RUNMODE): BufferedImage = {

    def writeImagePart(partId: String, bi: BufferedImage, imgPart: Seq[Seq[Int]], pos: Pos): Unit = {
      println(s"writeImagePart $partId")
      imgPart.zipWithIndex.foreach {
        case (row, i) => row.zipWithIndex.foreach {
          case (col, j) => bi.setRGB(pos.x + j, pos.y + i, col)
        }
      }
    }

    def writeImagePartTransp(partId: String, bi: BufferedImage, imgPart: Seq[Seq[Int]], pos: Pos): Unit = {
      println(s"writeImagePart transp $partId")
      imgPart.zipWithIndex.foreach {
        case (row, i) => row.zipWithIndex.foreach {
          case (col, j) => bi.setRGB(pos.x + i, pos.y + j, col)
        }
      }
    }

    def writeBackground(bi: BufferedImage): Unit = {
      val w = bi.getWidth
      val h = bi.getHeight
      val g = bi.getGraphics.asInstanceOf[Graphics2D]
      g.setBackground(Color.WHITE)
      g.fillRect(0, 0, w, h)
    }

    def writeLines(bi: BufferedImage, l: Int, b: Int, d: Int): Unit = {

      val g = bi.getGraphics.asInstanceOf[Graphics2D]

      case class P(x: Int, y: Int) {
        def add(p: P) = P(x + p.x, y + p.y)
      }

      def drawPoli(poli: Seq[P]): Unit = {
        poli.zip(poli.tail).foreach { case (f, t) =>
          g.drawLine(f.x, f.y, t.x, t.y)
        }
      }

      def drawBackground(): Unit = {
        val p = Seq(P(0, 0), P(0, l - 1), P(l - 1, l - 1), P(l - 1, 0))
          .map(_.add(P(b, b)))
          .map(_.add(P(l, 3 * l)))
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

      g.setColor(Color.BLACK)
      drawBackground()
      drawFlapVertLeft(P(l, 0))
      drawFlapHorDown(P(0, 2 * l))
      drawFlapVertLeft(P(l, 3 * l))
      drawFlapHorDown(P(l, 4 * l))
      drawFlapVertRight(P(2 * l, 3 * l))
      drawFlapVertRight(P(2 * l, 2 * l))
      drawFlapHorUp(P(2 * l, l))
    }

    def writeImagePartsSeq(bi: BufferedImage, pos: PartsPositions): Unit = {
      writeImagePart("C", bi, img.center, pos.center)
      writeImagePartTransp("L", bi, img.left, pos.left)
      writeImagePartTransp("R", bi, img.right, pos.right)
      writeImagePart("T", bi, img.top, pos.top)
      writeImagePart("B", bi, img.bottom, pos.bottom)
    }

    def writeImagePartsPar(bi: BufferedImage, pos: PartsPositions, timeout: Duration): Unit = {
      val fc = Future(writeImagePart("C", bi, img.center, pos.center))
      val fl = Future(writeImagePartTransp("L", bi, img.left, pos.left))
      val fr = Future(writeImagePartTransp("R", bi, img.right, pos.right))
      val ft = Future(writeImagePart("T", bi, img.top, pos.top))
      val fb = Future(writeImagePart("B", bi, img.bottom, pos.bottom))
      val r = for(rc <- fc; rl <- fl; rr <- fr; rt <- ft; rb <- fb) yield (rc, rl, rr, rt, rb)
      Await.ready(r, timeout)
     }

    val size = imageSize(img.partLen, border)
    val bi = new BufferedImage(size.w, size.h, BufferedImage.TYPE_INT_RGB)
    val pos: PartsPositions = partPositions(img.partLen, border)

    writeBackground(bi)

    runMode match {
      case RUNMODE_Seq => writeImagePartsSeq(bi, pos)
      case RUNMODE_Parallel(timeout) => writeImagePartsPar(bi, pos, timeout)
    }
    writeImagePartsSeq(bi, pos)

    writeLines(bi, img.partLen, border, percent(img.partLen, 15))

    bi
  }

  def writeImageToByteArray(bi: BufferedImage, mimeType: String): Array[Byte] = {
    import scala.collection.JavaConverters._

    val os = new ByteArrayOutputStream()
    val writers = ImageIO.getImageWritersByMIMEType(mimeType).asScala.toList
    val wrt = if (writers.nonEmpty) writers(0) else throw new IllegalStateException(s"Unknown image mime type'$mimeType'")
    val ios = ImageIO.createImageOutputStream(os)
    wrt.setOutput(ios)
    wrt.write(bi)
    os.close()
    os.toByteArray
  }

  def readImageFromStream(in: InputStream, mimeType: String): BufferedImage = {
    import scala.collection.JavaConverters._

    val readers = ImageIO.getImageReadersByMIMEType(mimeType).asScala.toList
    val reader = if (readers.nonEmpty) readers(0) else throw new IllegalStateException(s"Unknown image mime type'$mimeType'")
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
