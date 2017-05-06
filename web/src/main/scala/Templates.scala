package web.imagecube

sealed trait BGCOL { def txt: String }
case object BGCOL_normal extends BGCOL {def txt = "#ff9933"}
case object BGCOL_alarm extends BGCOL {def txt = "#ff8533"}

object Templates {
  
 def templ(content: String, bg: BGCOL): String = {
       s"""
<html>
<head>
<title>imagecube</title>
<style>
body {
    background-color: ${bg.txt};
    font-family: "Avant Garde", Avantgarde, "Century Gothic", CenturyGothic, AppleGothic, sans-serif;
    font-size: 200%;
    padding: 20px;
}
.b {
    font-weight: bold;		
}	
.s {
    font-size: 50%;
    text-align: right;
    padding-top: 20px;
}	
</style>
</head>
<body>
<h1>imagecube</h1>
$content
<p class="s" ><a href="http://entelijan.net">entelijan.net ... </a>
</body>
</html>  
   """
 }
  
def home: String = {
  """
        <p><a href="start">back to start ...</a></p>
  """
}
  
}