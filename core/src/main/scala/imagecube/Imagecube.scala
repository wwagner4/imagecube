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

object Imagecube {
  
  def transformImage(in: InputStream, inMime: String, outMime: String): Array[Byte] = {
    val bi = readImage(in, inMime)
    in.close
    val img = readImage(bi)  
    val shortImg = shortenImgPar(img)
    val biOut = createImage(shortImg, percent(img.partLen, 20))
    writeImage(biOut, outMime)
  }

  def writeImage(f: File, outDir: File): Unit = {
    try {
      val biIn = ImageIO.read(f)
      if (biIn == null) throw new IllegalStateException(s"$f seems not to contain image data")
      val img = readImage(biIn)  
      val shortImg = shortenImgPar(img)
      val biOut = createImage(shortImg, percent(img.partLen, 20))
      val fOutName = s"${extractName(f)}_out.png"
      val outFile = new File(outDir, fOutName)
      val typ = imageType(outFile)
      ImageIO.write(biOut, typ, outFile)
      println(s"wrote image to $outFile type: $typ")
    } catch {
      case e: Exception =>
        println(s"ERROR: Could not convert image ${f.getName} because $e")
    }
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

  def shortenImgSeq(img: Img): Img = {
    Img(
      img.partLen,
      img.center,
      shortenImagePartPar(img.left),
      shortenImagePartPar(img.right.reverse).reverse,
      shortenImagePartPar(img.top),
      shortenImagePartPar(img.bottom.reverse).reverse
    )
  }

  def shortenImgPar(img: Img): Img = {
    val fLeft = Future {
      shortenImagePartPar(img.left)
    }
    val fRight = Future {
      shortenImagePartPar(img.right.reverse).reverse
    }
    val fTop = Future {
      shortenImagePartPar(img.top)
    }
    val fBottom = Future {
      shortenImagePartPar(img.bottom.reverse).reverse
    }
    val r = for (rLeft <- fLeft; rRight <- fRight; rTop <- fTop; rBottom <- fBottom)
      yield (rLeft, rRight, rTop, rBottom)
    val (left, right, top, bottom) = Await.result(r, Duration(30, SECONDS))
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

  def shortenImagePartSeq(part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val newRowsA = shortenRowsA(part)
    val newRowsB = shortenRowsB(part)
    newRowsA.zip(newRowsB).map { case (a, b) => a ++ b }
  }

  def shortenImagePartPar(part: Seq[Seq[Int]]): Seq[Seq[Int]] = {

    val fRowsA = Future {
      shortenRowsA(part)
    }
    val fRowsB = Future {
      shortenRowsB(part)
    }
    val r = for (rRowsA <- fRowsA; rRowsB <- fRowsB) yield (rRowsA, rRowsB)
    val (newRowsA, newRowsB) = Await.result(r, Duration(30, SECONDS))
    newRowsA.zip(newRowsB).map { case (a, b) => a ++ b }
  }

  def shortenRowsA(part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    part.zipWithIndex.map { case (row, i) =>
      val n = part.size
      val (from, to) = shortenA(i, n)
      val filteredCol = row.zipWithIndex
        .filter { case (_, ir) => ir >= from && ir <= to }
        .map { case (c, _) => c }
      linearCompress(filteredCol, n / 2, colorMix)
    }
  }

  def shortenRowsB(part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
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

  def createImage(img: Img, border: Int): BufferedImage = {

    def writeImagePart(bi: BufferedImage, imgPart: Seq[Seq[Int]], pos: Pos): Unit = {
      imgPart.zipWithIndex.foreach {
        case (row, i) => row.zipWithIndex.foreach {
          case (col, j) => bi.setRGB(pos.x + j, pos.y + i, col)
        }
      }
    }

    def writeImagePartTransp(bi: BufferedImage, imgPart: Seq[Seq[Int]], pos: Pos): Unit = {
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

      def drawGround(): Unit = {
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
      drawGround()
      drawFlapVertLeft(P(l, 0))
      drawFlapHorDown(P(0, 2 * l))
      drawFlapVertLeft(P(l, 3 * l))
      drawFlapHorDown(P(l, 4 * l))
      drawFlapVertRight(P(2 * l, 3 * l))
      drawFlapVertRight(P(2 * l, 2 * l))
      drawFlapHorUp(P(2 * l, l))
    }

    val size = imageSize(img.partLen, border)
    val bi = new BufferedImage(size.w, size.h, BufferedImage.TYPE_INT_RGB)
    writeBackground(bi)
    val pos = partPositions(img.partLen, border)

    writeImagePart(bi, img.center, pos.center)
    writeImagePartTransp(bi, img.left, pos.left)
    writeImagePartTransp(bi, img.right, pos.right)
    writeImagePart(bi, img.top, pos.top)
    writeImagePart(bi, img.bottom, pos.bottom)

    writeLines(bi, img.partLen, border, percent(img.partLen, 15))

    bi
  }
  
  def writeImage(bi: BufferedImage, mimeType: String): Array[Byte] = {
    import scala.collection.JavaConverters._

    val os = new ByteArrayOutputStream()
    val writers = ImageIO.getImageWritersByMIMEType(mimeType).asScala.toList
    val wrt = if (!writers.isEmpty) writers(0) else throw new IllegalStateException(s"Unknown image mime type'$mimeType'") 
    val ios = ImageIO.createImageOutputStream(os);
    wrt.setOutput(ios);
    wrt.write(bi);
    os.close();
    os.toByteArray
  }
  
  def readImage(in: InputStream, mimeType: String): BufferedImage = {
    import scala.collection.JavaConverters._
    
    val readers = ImageIO.getImageReadersByMIMEType(mimeType).asScala.toList
    val reader = if (!readers.isEmpty) readers(0) else throw new IllegalStateException(s"Unknown image mime type'$mimeType'")   
    val iis=ImageIO.createImageInputStream(in);
    reader.setInput(iis);
    reader.read(reader.getMinIndex)
  }


  def percent(value: Int, perc: Int): Int = (value.toDouble * perc / 100.0).round.toInt

  def extractName(f: File): String = {
    val i = f.getName.lastIndexOf('.')
    if (i < 0) f.getName
    else f.getName().substring(0, i)
  }


}
