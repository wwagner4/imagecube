package web.imagecube

import org.scalatra._
import servlet.{MultipartConfig, SizeConstraintExceededException, FileUploadSupport}
import xml.Node
import Templates._

import imagecube._
import imagecube.Imagecube._

import scala.concurrent.duration._
import scala.concurrent.duration.Duration._

class ImagecubeServlet extends ScalatraServlet with FileUploadSupport with FlashMapSupport {
  
  val limit = 1
  val runmode = RUNMODE_Parallel(Duration(30, SECONDS))
  
  configureMultipartHandling(MultipartConfig(maxFileSize = Some(limit *1024*1024)))
  
    error {
    case e: SizeConstraintExceededException =>
          contentType="text/html"
        val content = s"""
        <p class="b">The file you uploaded exceeded the ${limit} MB limit.</p>
        $home
        """
        templ(content, BGCOL_alarm)
  }

  get("/") {
    contentType="text/html"
    
    val content = s"""
      <p>Transform your images to cubes</p>
      <form action="/upload" method="post" enctype="multipart/form-data">
       <p>
       
       <label for="file-upload" class="button">Select a file</label>
       <input id="file-upload"  class="button" type="file" name="file" value = "select file"/>
        </p>
        
       <p><input class="button" type="submit" value="transform" /></p>
      </form>
      <p>
        Select a file. After you hit "transform"
        an imagecube will be created and downloaded to your computer
      </p>
      <p>The maximum file size accepted is ${limit} MB</p>
      <p>See some cubes at the <a target="_blank" href="http://imgur.com/a/iTPwP">cube gallery (imgur) ... </a></p>

    """
    templ(content, BGCOL_normal)
  }
  post("/upload") {
    import imagecube._
    contentType="text/html"
    fileParams.get("file") match {
      case Some(file) =>
        if (file.getSize == 0) {
          templ(contentError("Hey! You forgot to select a file"), BGCOL_alarm)
        } else {
          try {
            val mime = file.contentType.getOrElse("application/octet-stream")
            val transformed = transformImage(file.getInputStream, mime, mime, runmode)
            Ok(transformed, Map(
              "Content-Type"        -> (mime),
              "Content-Disposition" -> ("attachment; filename=\"" + file.name + "\"")
            ))
          } catch {
            case e: Exception => 
              e.printStackTrace
              templ(contentError(s"Error transforming file: ${e.getMessage}"), BGCOL_alarm)
          }
        }
      case None =>
        templ(contentError("Hey! You forgot to select a file"), BGCOL_alarm)
    }
  }
  


}
