package imagecube

import java.io.File
import javax.imageio.ImageIO

import imagecube.Imagecube._


object Main extends App {

  runDir()

  def runDir(): Unit = {

    val inDirPath = "tmp/cubes/in"
    val outDirPath = "tmp/cubes/out"
    val handed = HANDED_Right
    val cutLines = false

    val homeDirStr = System.getProperty("user.home")
    val homeDir = new File(homeDirStr)
    val inDir = new File(homeDir, inDirPath)
    if (!inDir.exists()) throw new IllegalStateException(s"Input directory '$inDir' does not exist")
    val outDir = new File(homeDir, outDirPath)
    outDir.mkdirs()
    val files = inDir.listFiles()
    val start = System.nanoTime()
    files.foreach { f =>
      if (f.isFile && !f.getName.startsWith(".")) {
        writeImage(f, outDir, handed, cutLines)
      }
    }
    val stop = System.nanoTime()
    val time = (stop - start).toDouble / 1000000000L
    println(f"FINISHED runDir $time%.2f s")

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
