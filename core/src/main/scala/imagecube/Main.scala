package imagecube

import java.io.File
import javax.imageio.ImageIO

import imagecube.Imagecube._


case class Config(inDirPath: String = "tmp/cubes/in",
                  outDirPath: String = "tmp/cubes/out",
                  handed: HANDED = HANDED_Right,
                  cutLines: Boolean = false
                 )

object Main {

  def main(args: Array[String]): Unit = runDir(args)

  def runDir(args: Array[String]): Unit = {

    readCommandline(args)


  }

  def readCommandline(args: Array[String]): Unit = {
    val paramsStr = args.mkString("<", "|", ">")
    println("Commandline: " + paramsStr)

    val parser = new scopt.OptionParser[Config]("java -jar imagecube.jar") {
      head("imagecube", "1.0")

      opt[String]('i', "inDirPath").action((x, c) =>
        c.copy(inDirPath = x)).text("path to the input directory")

      opt[String]('o', "outDirPath").action((x, c) =>
        c.copy(outDirPath = x)).text("path to the output directory")

      opt[String]('h', "handed").action((x, c) =>
        c.copy(handed = strToHanded(x))).text("path to the input directory")

      opt[Unit]('c', "cutLines").action((_, c) =>
        c.copy(cutLines = true)).text("draw extra lines for cutting")

    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val homeDirStr = System.getProperty("user.home")
        val homeDir = new File(homeDirStr)
        val inDir = new File(homeDir, config.inDirPath)
        if (!inDir.exists()) throw new IllegalStateException(s"Input directory '$inDir' does not exist")
        val outDir = new File(homeDir, config.outDirPath)
        outDir.mkdirs()
        val files = inDir.listFiles()
        val start = System.nanoTime()
        files.foreach { f =>
          if (f.isFile && !f.getName.startsWith(".")) {
            writeImage(f, outDir, config.handed, config.cutLines)
          }
        }
        val stop = System.nanoTime()
        val time = (stop - start).toDouble / 1000000000L
        println(f"FINISHED runDir $time%.2f s")
      case None => // Nothing to do
    }

    Config()
  }

  def strToHanded(str: String): HANDED = {
    str match {
      case "r" => HANDED_Right
      case "l" => HANDED_Left
      case _ => throw new IllegalArgumentException(s"Invalid value '$str' for option handed (-h). Valid is 'r' for right or 'l for left'")
    }
  }


  def writeImage(f: File, outDir: File, handed: HANDED, cutLines: Boolean): Unit = {
    try {
      val biIn = ImageIO.read(f)
      if (biIn == null) throw new IllegalStateException(s"$f seems not to contain image data")
      val biOut = transformImage(biIn, handed, cutLines)
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


}
