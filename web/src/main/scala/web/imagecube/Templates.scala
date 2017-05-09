package web.imagecube

sealed trait BGCOL {
  def txt: String
}

case object BGCOL_normal extends BGCOL {
  def txt = "#ff9933"
}

case object BGCOL_alarm extends BGCOL {
  def txt = "#ff8533"
}

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
    padding: 10px;
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
    padding: 20px 70px;
    text-align: center;
    text-decoration: none;
    display: inline-block;
    margin: 4px 2px;
    cursor: pointer;
    border-radius: 12px;
}
input[type="file"] {
    display: none;
}

a:link {
    color: #4d1f00;
}
a:visited {
    color: #4d1f00;
}
a:hover {
    color: #993d00;
}
a:active {
    color: #993d00;
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
        <p><a href="start">back to start ...</a></p>
  """
  }

  def contentError(text: String): String = {
    s"""
          <p  class="b">$text</p>
        $home
  """
  }

}