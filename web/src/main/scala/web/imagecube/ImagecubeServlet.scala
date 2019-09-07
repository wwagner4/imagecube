package web.imagecube

import org.scalatra._
import org.scalatra.servlet.FileUploadSupport

import imagecube.Imagecube._
import web.imagecube.Templates._

class ImagecubeServlet extends ScalatraServlet with FileUploadSupport with FlashMapSupport {

  val limit = 30

  //val ctx = "/imagecube"
  val ctx = ""

  val uploadUrlStr = "/upload"
  val startUrlStr = "/"

  get("/") {
    contentType = "text/html"
    val uploadUrl = s"$ctx$uploadUrlStr"

    val content =
      s"""
      <p>Transform your images to cubes (<a target="_blank" href="https://flic.kr/s/aHskVeDKWN">Examples...</a>)</p>
      <form id="myForm" action="$uploadUrl" method="post" enctype="multipart/form-data">
       <p>

       <label for="file-upload" class="button">Select an image</label>
       <input id="file-upload"  class="button" type="file" name="file" value = "select file" onchange="document.getElementById('myForm').submit();"/>
        </p>

      </form>
      <p>
        After you select an image
        a cube will be created and immediately downloaded.
      </p>
      <p>The maximum image size accepted is $limit m pixel</p>
    """
    templ(content, BGCOL_normal)
  }
  post("/upload") {
    val startUrl = s"$ctx$startUrlStr"
    fileParams.get("file") match {
      case Some(file) =>
        if (file.getSize == 0) {
          contentType = "text/html"
          templ(contentError("Hey! You forgot to select a file", startUrl), BGCOL_alarm)
        } else {
          try {
            val mime = file.contentType.getOrElse("application/octet-stream")
            val transformed = transformImageWeb(file.getInputStream, mime, mime, limit)
            Ok(transformed, Map.apply(
              "Content-Type" -> mime,
              "Content-Disposition" -> ("attachment; filename=\"" + file.name + "\"")
            ))
          } catch {
            case e: Exception =>
              contentType = "text/html"
              e.printStackTrace()
              templ(contentError(s"Error transforming file: ${e.getMessage}", startUrl), BGCOL_alarm)
          }
        }
      case None =>
        contentType = "text/html"
        templ(contentError("Hey! You forgot to select a file", startUrl
        ), BGCOL_alarm)
    }
  }


}
