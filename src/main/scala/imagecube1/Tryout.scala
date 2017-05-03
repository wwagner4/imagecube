package imagecube1

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import Imagecube._


object Tryout extends App {

  // readFile()
  // createCutParams()
  // transform()
  // runFibs()
  // diffs()
  // compress()
  // mean()
  // rgb()
  // shorten()
  // shorten1()
  // positionParts()
  writeImage()

  def writeImage(): Unit = {

    val names1 = List(
      "cow",
      "cow1",
      "big",
      "big1"
    )

    val names = List(
      "cow"
    )

    val startTime = System.currentTimeMillis()
    names.par.foreach{ name =>
      val fName = s"$name.jpg"
      val fOutName = s"${name}_out.png"
      val f = new File(dir, fName)
      val img = readImage(f)

      println(s"created img for $fName")

      val shortImg = shortenImg(img)

      val bi = createImage(shortImg, 5)

      val outFile = new File(tmpdir, fOutName)
      val typ = imageType(outFile)
      ImageIO.write(bi, typ, outFile)

      println(s"wrote image to $outFile type: $typ")
    }
    val stopTime = System.currentTimeMillis()
    println(s"time: ${stopTime - startTime}")
  }

  def positionParts(): Unit = {

    val partLen = 2
    val border = 1

    val s = imageSize(partLen, border)
    val pp = partPositions(partLen, border)

    println(s)
    println(pp)
  }

  def shorten1(): Unit = {

    val fName = "cow.jpg"
    val f = new File(dir, fName)
    val img = readImage(f)

    println(s"created img for $fName")

    val shortImg = shortenImg(img)
    println(s"shorted image from $fName - partLen: ${shortImg.partLen}")
  }

  def shorten(): Unit = {

    val n = 10

    val is = 0 to n
    is.foreach { i =>
      val (fa, ta) = shortenA(i, n)
      val (fb, tb) = shortenB(i, n)
      val la = ta - fa
      val lb = tb - fb
      println(f"shorten   $n%3d $i%3d : $fa%3d - $ta%3d ($la%3d) : $fb%3d - $tb%3d ($lb%3d)")
    }
  }

  def rgb(): Unit = {

    val colors = List(
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.BLUE,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED,
      Color.RED
    )

    val ints = colors.map(_.getRGB)

    val re = colorMix(ints)

    println(re)
    val c = new Color(re)
    println(c)
    println(Color.RED.getRGB)

  }

  def mean(): Unit = {

    def m(a: Double, b: Double) = (a + b) / 2

    val y1 = m(m(1, 2), 3)
    val y2 = m(m(1, 3), 2)
    val y3 = m(m(3, 2), 1)

    println(y1)
    println(y2)
    println(y3)
  }

  def transform(): Unit = {

    def diff(s: Stream[Int]): Stream[Int] = {
      s.zip(s.tail).map { case (a, b) => b - a }
    }

    val n = 10
    val ms = n to(3 * n, 1)

    ms.foreach { m =>
      val v = m.toDouble / n
      val borders = Stream.iterate(0.0)(x => x + v).map(x => x.round.toInt)
      println(f"$n%5d, $m%5d, $v%5.6f")
      println("   " + borders.map(format).take(n + 1).mkString(", "))
      println("     " + diff(borders).map(format).take(n).mkString(", "))
      println
    }

    def format(v: Int): String = f"$v%3d"

  }

  def compress(): Unit = {

    def mean(in: Seq[Double]): Double = in.sum / in.size

    val in = List(1, 2, 3, 4, 2, 22, 23, 3, 24, 25, 72, 2, 2, 2, 2, 2, 2, 4, 2).map(_.toDouble)

    (1 to in.size).foreach(n => println(linearCompress(in, n, mean).mkString(", ")))

  }

  lazy val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }


  def diffs(): Unit = {
    def diff(s: Stream[Int]): Stream[Int] =
      s.zip(s.tail).map { case (a, b) => b - a }

    println("FIBS: " + fibs.take(10).mkString(", "))
    println("DIFFS: " + diff(fibs).take(10).mkString(", "))
  }

  def runFibs(): Unit = {
    println(fibs.take(30).mkString(", "))
  }


  def createCutParams(): Unit = {
    val whs = Seq(
      (5, 6),
      (5, 7),
      (5, 8),
      (6, 6),
      (7, 6),
      (8, 6)
    )

    whs.foreach {
      case (w, h) => println(s"($w, $h, ${cutParams(w, h)}, (Range(0, 0), Range(0, 0))),")
    }
  }

  def readFile(): Unit = {
    val fName = "tiny1.jpg"
    val f = new File(dir, fName)
    val img = readImage(f).toString.take(100) + " ..."
    println(s"created img for $fName")
    println(s"img: $img")
  }


  def dir: File = new File("src/main/resources")

  def tmpdir: File = {
    val re = new File("target/tmp")
    if (!re.exists()) re.mkdirs()
    re
  }


}
