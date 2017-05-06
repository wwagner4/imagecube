package web.imagecube

import org.scalatra._
import servlet.{MultipartConfig, SizeConstraintExceededException, FileUploadSupport}
import xml.Node
import Templates._

class ImagecubeServlet extends ScalatraServlet with FileUploadSupport with FlashMapSupport {
  
  val limit = 20
  
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
      <form action="/upload" method="post" enctype="multipart/form-data">
       <p>File to be transformed <input type="file" name="file" value = "select file"/></p>
       <p><input type="submit" value="transform" /></p>
      </form>
      <p>
        Select a file. After you hit "transform"
        an imagecube will be created and downloaded to your computer
      </p>

      <p> The maximum file size accepted is ${limit} MB</p>
    """
    templ(content, BGCOL_normal)
  }
  post("/upload") {
    fileParams.get("file") match {
      case Some(file) =>
        println(s"found a file - $file - size:${file.getSize} - type:${file.contentType.getOrElse("???")}")
        if (file.getSize == 0) {
          contentType="text/html"
          val content = s"""
          <p  class="b">Hey! You forgot to select a file.</p>
          $home
          """
          templ(content, BGCOL_alarm)
        } else {
          Ok(file.get(), Map(
            "Content-Type"        -> (file.contentType.getOrElse("application/octet-stream")),
            "Content-Disposition" -> ("attachment; filename=\"" + file.name + "\"")
          ))
        }
      case None =>
        contentType="text/html"
        val content = s"""
        <p  class="b">Hey! You forgot to select a file.</p>
        $home
        """
        templ(content, BGCOL_alarm)
    }
  }
  


}
