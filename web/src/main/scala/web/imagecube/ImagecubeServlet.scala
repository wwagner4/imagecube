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
    val header = s"""
<script type="text/javascript" src="//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>
<script type="text/javascript" src="js/simpleUpload.min.js"></script>
<script type="text/javascript">
$$(document).ready(function(){
	$$('input[type=file]').change(function(){
		$$(this).simpleUpload("/upload", {
			start: function(file){
				//upload started
				console.log("upload started");
			},
			progress: function(progress){
				//received progress
				console.log("upload progress: " + Math.round(progress) + "%");
			},
			success: function(data){
				//upload successful
				console.log("upload successful!");
				//console.log(data);
				$$('.div_imagetranscrits').html('<img src="data:image/jpeg;base64,' + data + '" width=400/>');
			},
			error: function(error){
				//upload failed
				console.log("upload error: " + error.name + ": " + error.message);
			}
		});
	});
});
</script>
    """
    val content = s"""
      <p>Transform your images to cubes</p>
      <input type="file" name="file">
      <p><div class="div_imagetranscrits"></div></p>
      <p>
        Select a file. After you hit "transform"
        an imagecube will be created and downloaded to your computer
      </p>
      <p>The maximum file size accepted is ${limit} MB</p>
      <p>See some cubes at the <a target="_blank" href="http://imgur.com/a/iTPwP">cube gallery (imgur) ... </a></p>

    """
    templ(content, BGCOL_normal, header)
  }
  post("/upload") {
    import imagecube._
    import java.util.Base64
    contentType="text/html"
    fileParams.get("file") match {
      case Some(file) =>
        if (file.getSize == 0) {
          templ(contentError("Hey! You forgot to select a file"), BGCOL_alarm)
        } else {
          try {
            val mime = file.contentType.getOrElse("application/octet-stream")
            val transformed = Base64.getEncoder.encodeToString(transformImage(file.getInputStream, mime, mime, runmode))
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
