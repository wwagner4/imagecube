package imagecube

import java.awt.Color
import java.io._
import javax.imageio._

import Imagecube._

import scala.concurrent.duration._
import scala.concurrent.duration.Duration._


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
  // parallel()
  // perc()
  // runExtractName()
  // runTestImages()
  // readWriteStream()
  runDir()

  def runDir(): Unit = {

    def writeImage(f: File, outDir: File, runMode: RUNMODE): Unit = {
      try {
        val biIn = ImageIO.read(f)
        if (biIn == null) throw new IllegalStateException(s"$f seems not to contain image data")
        val img = readImage(biIn)
        val shortImg = runMode match {
          case RUNMODE_Seq => shortenImgSeq(img)
          case RUNMODE_Parallel(timeout) => shortenImgPar(img, timeout)
        }
        val biOut = createImage(shortImg, percent(img.partLen, 20), runMode)
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

    val homeDirStr = System.getProperty("user.home")
    val homeDir = new File(homeDirStr)


    val inDir = new File(homeDir, "tmp/cubes/in")
    if (!inDir.exists()) throw new IllegalStateException(s"Input directory '$inDir' does not exist")

    val outDir = new File(homeDir, "tmp/cubes/out")
    //val runMode = RUNMODE_Parallel(Duration(20, SECONDS))
    val runMode = RUNMODE_Seq

    outDir.mkdirs()
    val files = inDir.listFiles()
    val start = System.nanoTime()
    files.foreach { f =>
      if (f.isFile) {
        writeImage(f, outDir, runMode)
      }
    }
    val stop = System.nanoTime()
    val time = (stop - start).toDouble / 1000000000L
    println(f"FINISHED runDir $time%.2f s")

  }

  def readWriteStream(): Unit = {

    def transformImage(in: InputStream, inMime: String, outMime: String, runMode: RUNMODE): Array[Byte] = {
      val bi = readImage(in, inMime)
      in.close()
      val img = readImage(bi)
      val shortImg = runMode match {
        case RUNMODE_Seq => shortenImgSeq(img)
        case RUNMODE_Parallel(timeout) => shortenImgPar(img, timeout)
      }
      val biOut = createImage(shortImg, percent(img.partLen, 20), runMode)
      writeImage(biOut, outMime)
    }

    val runMode = RUNMODE_Seq
    val fNam = "cow.jpg"
    val f = new File(dir, fNam)
    val in = new FileInputStream(f)

    val bytes = transformImage(in, "image/jpeg", "image/png", runMode)

    val out = new File(tmpdir, "so_cow.png")
    val target = new BufferedOutputStream(new FileOutputStream(out))
    try bytes.foreach(target.write(_)) finally target.close()
    println(s"wrote to $out")
  }

  def runExtractName(): Unit = {
    Seq(
      new File("a/b/c.png"),
      new File("a/b/c"),
      new File("a/b/cksjdfhskd_kasdjsal.png"),
      new File("a/b/c.jpg.k")
    ).foreach { f =>
      val name = extractName(f)
      println(s"$f -> $name")
    }
  }

  def perc(): Unit = {
    val value = 1000
    (1 to 20).foreach { p =>
      val value1 = percent(value, p)
      println(s"$value $p% -> $value1")
    }
  }

  def positionParts(): Unit = {

    val partLen = 2
    val border = 1

    val s = imageSize(partLen, border)
    val pp = partPositions(partLen, border)

    println(s)
    println(pp)
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

  def parallel(): Unit = {
    val ran = new java.util.Random()

    def job(id: String, steps: Int): Int = {
      println(f"JOB_$id STARTED")
      (1 to steps).foreach { i =>
        Thread.sleep(5 + ran.nextInt(95))
        println(f"JOB_$id $i%3d/$steps%3d")
      }
      println(f"JOB_$id READY")
      steps
    }

    import scala.concurrent._
    import ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val f1 = Future {
      job("A", 50)
    }
    val f2 = Future {
      job("B", 10)
    }
    val r: Future[(Int, Int)] = for (r1 <- f1; r2 <- f2) yield (r1, r2)
    val (a, b): (Int, Int) = Await.result(r, Duration(10, SECONDS))
    println(s"ready $a $b")
  }

  def dir: File = new File("core/src/test/resources")

  def tmpdir: File = {
    val re = new File("core/target/tmp")
    if (!re.exists()) re.mkdirs()
    re
  }


}
