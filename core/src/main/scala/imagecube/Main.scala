package imagecube

import java.io.File
import javax.imageio.ImageIO

import imagecube.Imagecube._


object Config {
  def homeDir = new File(System.getProperty("user.home"))

  def workDir = new File(homeDir, "imagecube")
}

case class Config(inDir: File = new File(Config.workDir, "in"),
                  outDir: File = new File(Config.workDir, "out"),
                  handed: HANDED = HANDED_Right,
                  cutLines: Boolean = false
                 )


object Main {

  def main(args: Array[String]): Unit = runDir(args)

  def runDir(args: Array[String]): Unit = {

    readCommandline(args)


  }

  def readCommandline(args: Array[String]): Unit = {

    val parser = new scopt.OptionParser[Config]("java -jar imagecube.jar") {
      head("imagecube", "1.0")

      opt[File]('i', "inDir").action((x, c) =>
        c.copy(inDir = x)).text("Input directory. Default is $HOME/imagecube/in.")

      opt[File]('o', "outDir").action((x, c) =>
        c.copy(outDir = x)).text("Output directory. Default is $HOME/imagecube/out.")

      opt[String]('h', "handed").action((x, c) =>
        c.copy(handed = strToHanded(x))).text("Defines if the lashes are right or left handed. Values 'r' or 'l'. Default is 'r'.")

      opt[Unit]('c', "cutLines").action((_, c) =>
        c.copy(cutLines = true)).text("Draw extra lines for cutting. Default is no extra lines. Set this option if your image(s) are very light.")

    }

    parser.parse(args, Config()) match {
      case Some(config) => writeImage(config)
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

  def writeImage(conf: Config): Unit = {
    try {
      println(f"START processing images in ${conf.inDir}")
      if (!conf.inDir.exists()) throw new IllegalStateException(s"Input directory '${conf.inDir}' does not exist")
      conf.outDir.mkdirs()
      val files = conf.inDir.listFiles()
      files.foreach { f =>
        if (f.isFile && !f.getName.startsWith(".")) {
          writeImage(f, conf.outDir, conf.handed, conf.cutLines)
        }
      }
      println(f"FINISHED processing images in ${conf.inDir}")
    } catch {
      case e: Exception =>
        println(s"ERROR: Could not create imagecubes because: ${e.getMessage}")
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
        println(s"ERROR: Could not convert image ${f.getName} because: ${e.getMessage}")
    }
  }


}
