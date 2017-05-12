package web.imagecube

import org.scalatra._
import org.scalatra.servlet.{MultipartConfig, SizeConstraintExceededException, FileUploadSupport}

import imagecube.Imagecube._
import web.imagecube.Templates._

class ImagecubeServlet extends ScalatraServlet with FileUploadSupport with FlashMapSupport {

  val limit = 1

  configureMultipartHandling(MultipartConfig.apply(maxFileSize = Some.apply(limit * 1024 * 1024)))

  error {
    case _: SizeConstraintExceededException =>
      contentType = "text/html"
      templ(contentError(s"The image you uploaded exceeded the $limit MB limit.", url("/start")), BGCOL_alarm)
  }

  get("/start") {
    contentType = "text/html"

    val content =
      s"""
      <p>Transform your images to cubes</p>
      <form id="myForm" action="${url("/upload")}" method="post" enctype="multipart/form-data">
       <p>

       <label for="file-upload" class="button">Select an image</label>
       <input id="file-upload"  class="button" type="file" name="file" value = "select file" onchange="document.getElementById('myForm').submit();"/>
        </p>

      </form>
      <p>
        After you select an image
        a cube will be created and immediately downloaded.
      </p>
      <p>The maximum file size accepted is $limit MB.</p>
      <p>
      <a data-flickr-embed="true"  href="https://www.flickr.com/photos/148922320@N05/albums/72157680842147822" title="imagecube"><img src="https://c1.staticflickr.com/5/4182/34500371176_d564fcaae1_z.jpg" width="640" height="555" alt="imagecube"></a><script async src="//embedr.flickr.com/assets/client-code.js" charset="utf-8"></script>
      </p>

    """
    templ(content, BGCOL_normal)
  }
  post("/upload") {
    fileParams.get("file") match {
      case Some(file) =>
        if (file.getSize == 0) {
          contentType = "text/html"
          templ(contentError("Hey! You forgot to select a file", url("/start")), BGCOL_alarm)
        } else {
          try {
            val mime = file.contentType.getOrElse("application/octet-stream")
            val transformed = transformImageWeb(file.getInputStream, mime, mime)
            Ok.apply(transformed, Map.apply(
              "Content-Type" -> mime,
              "Content-Disposition" -> ("attachment; filename=\"" + file.name + "\"")
            ))
          } catch {
            case e: Exception =>
              contentType = "text/html"
              e.printStackTrace()
              templ(contentError(s"Error transforming file: ${e.getMessage}", url("/start")), BGCOL_alarm)
          }
        }
      case None =>
        contentType = "text/html"
        templ(contentError("Hey! You forgot to select a file", url("/start")), BGCOL_alarm)
    }
  }


}
