package imagecube

import java.io.File
import javax.imageio.ImageIO

import imagecube.Imagecube._


case class Params(inDirPath: String = "tmp/cubes/in",
                  outDirPath: String = "tmp/cubes/out",
                  handed: HANDED = HANDED_Right,
                  cutLines: Boolean = false
                 )

object Main {

  def main( args:Array[String] ):Unit = runDir(args)

  def runDir(args: Array[String]): Unit = {

    val params = readCommandline(args)

    val homeDirStr = System.getProperty("user.home")
    val homeDir = new File(homeDirStr)
    val inDir = new File(homeDir, params.inDirPath)
    if (!inDir.exists()) throw new IllegalStateException(s"Input directory '$inDir' does not exist")
    val outDir = new File(homeDir, params.outDirPath)
    outDir.mkdirs()
    val files = inDir.listFiles()
    val start = System.nanoTime()
    files.foreach { f =>
      if (f.isFile && !f.getName.startsWith(".")) {
        writeImage(f, outDir, params.handed, params.cutLines)
      }
    }
    val stop = System.nanoTime()
    val time = (stop - start).toDouble / 1000000000L
    println(f"FINISHED runDir $time%.2f s")

  }

  def readCommandline(args: Array[String]): Params = {
    val paramsStr = args.mkString("<", "|", ">")
    println("Commandline: " + paramsStr)
    Params()
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
