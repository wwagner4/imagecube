package web.imagecube

import org.scalatra._
import servlet.{MultipartConfig, SizeConstraintExceededException, FileUploadSupport}
import xml.Node

class ImagecubeServlet extends ScalatraServlet with FileUploadSupport with FlashMapSupport {
  
  val limit = 20
  
  configureMultipartHandling(MultipartConfig(maxFileSize = Some(limit *1024*1024)))
  
    error {
    case e: SizeConstraintExceededException =>
      RequestEntityTooLarge(
    <html>
      <body>
        <p>The file you uploaded exceeded the {limit} MB limit.</p>
      </body>
    </html>
    )
  }


  get("/") {
    <html>
      <body>
        <h1>imagecube</h1>
        <a href="ul">go to upload</a>.
      </body>
    </html>
  }
  get("/ul") {
    <html>
      <body>
        <h1>upload page</h1>
      <form action={url("/upload")} method="post" enctype="multipart/form-data">
       <p>File to upload: <input type="file" name="file" value = "select file"/></p>
       <p><input type="submit" value="upload" /></p>
      </form>
      <p>
        Upload a file using the above form. After you hit "Upload"
        the file will be uploaded and your browser will start
        downloading it.
      </p>

      <p>
        The maximum file size accepted is {limit} MB.
      </p>
      </body>
    </html>
  }
    post("/upload") {
    fileParams.get("file") match {
      case Some(file) =>
        println(s"found a file - $file - size:${file.getSize} - type:${file.contentType.getOrElse("???")}")
        if (file.getSize == 0) {
          BadRequest(
      <html>
        <body>
            <p>
              Hey! You forgot to select a file.
            </p>
          <p>
            <a href="/">home</a>.
          </p>
        </body>
      </html>
            )
        } else {
        Ok(file.get(), Map(
          "Content-Type"        -> (file.contentType.getOrElse("application/octet-stream")),
          "Content-Disposition" -> ("attachment; filename=\"" + file.name + "\"")
        ))
        }
        

      case None =>
        println("bad request - NO FILE")
        BadRequest(
    <html>
      <body>
          <p>
            Hey! You forgot to select a file.
          </p>
          <p>
            <a href="/">home</a>.
          </p>
      </body>
    </html>
          )
    }
  }

}
