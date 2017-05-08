package web.imagecube

sealed trait BGCOL { def txt: String }
case object BGCOL_normal extends BGCOL {def txt = "#ff9933"}
case object BGCOL_alarm extends BGCOL {def txt = "#ff8533"}

object Templates {
  
 def templ(content: String, bg: BGCOL, additionalHeaderContent: String = ""): String = {
       s"""
<html>
<head>
<title>imagecube</title>
<link href="https://fonts.googleapis.com/css?family=Russo+One" rel="stylesheet">
<style>
body {
    background-color: ${bg.txt};
    font-family: 'Russo One', sans-serif;
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
.button {
    background-color: #ffd1b3;
    border: none;
    color: black;
    padding: 25px 40px;
    text-align: center;
    text-decoration: none;
    display: inline-block;
    font-size: 16px;
    margin: 4px 2px;
    cursor: pointer;
    border-radius: 12px;
}
</style>
$additionalHeaderContent
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
  s"""
        <p><a href="/">back to start ...</a></p>
  """
}

def contentError(text: String): String = {
  s"""
          <p  class="b">$text</p>
        $home
  """
}
  
}