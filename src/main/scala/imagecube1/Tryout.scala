package imagecube1

import java.awt.Color
import java.io.File


object Tryout extends App {

  def dir: File = new File("src/main/resources")

  def tmpdir: File = {
    val re = new File("target/tmp")
    if (!re.exists()) re.mkdirs()
    re
  }

  // readFile()
  // createCutParams()
  // transform()
  // runFibs()
  // diffs()
  // compress()
  // mean()
  rgb()

  def rgb(): Unit = {

    import Imagecube._

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

    import Imagecube._

    def mean(in: Seq[Double]): Double = in.sum / in.size

    val in = List(1, 2, 3, 4, 2, 22, 23, 3, 24, 25, 72, 2, 2, 2, 2, 2, 2, 4, 2).map(_.toDouble)

    (1 to in.size).foreach(n => println(linearCompress(in, n, mean).mkString(", ")))

  }

  lazy val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }


  def diffs(): Unit = {
    def diff(s: Stream[Int]): Stream[Int] = {
      s.zip(s.tail).map { case (a, b) => b - a }
    }

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
      case (w, h) => println(s"($w, $h, ${ImagecubeUtil.cutParams(w, h)}, (Range(0, 0), Range(0, 0))),")
    }
  }

  def readFile(): Unit = {
    import Imagecube._
    val fName = "tiny1.jpg"
    val f = new File(dir, fName)
    val img = readImage(f).toString.take(100) + " ..."
    println(s"created img for $fName")
    println(s"img: $img")
  }

}