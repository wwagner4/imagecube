package imagecube

import java.io.File

import org.scalatest._

class ImagecubeSuite extends FunSuite {

  import Imagecube._


  val mixValues = List(
    (Col(0, 255, 0), Col(0, 255, 0), Col(0, 255, 0)),
    (Col(1, 255, 20), Col(1, 255, 10), Col(1, 255, 15)),
    (Col(1, 255, 0), Col(3, 255, 0), Col(2, 255, 0))
  )
  mixValues.foreach { case (p1, p2, pr) =>
    test(s"Mix two pixel $p1 $p2") {
      assert(mix(p1, p2) === pr)
    }
  }

  def image23: Image = {
    val px = Seq(
      Col(0, 3, 4),
      Col(1, 3, 4),
      Col(2, 3, 4),
      Col(3, 3, 4),
      Col(4, 3, 4),
      Col(5, 3, 4)
    )
    Image(2, 3, px)
  }

  test("pixel") {
    val p = pixel(image23)
    assert(p.size === 6)
    assert(p(0) === Pixel(0, 0, Col(0, 3, 4)))
    assert(p(1) === Pixel(1, 0, Col(1, 3, 4)))
    assert(p(2) === Pixel(0, 1, Col(2, 3, 4)))
    assert(p(3) === Pixel(1, 1, Col(3, 3, 4)))
    assert(p(4) === Pixel(0, 2, Col(4, 3, 4)))
    assert(p(5) === Pixel(1, 2, Col(5, 3, 4)))
  }

  Seq(
    (0, Seq(Col(0, 3, 4), Col(1, 3, 4))),
    (1, Seq(Col(2, 3, 4), Col(3, 3, 4))),
    (2, Seq(Col(4, 3, 4), Col(5, 3, 4)))
  ).foreach { case (i, should) =>
    test(s"rows$i") {
      assert(rows(image23)(i) === should)
    }
  }

  Seq(
    (0, Seq(Col(0, 3, 4), Col(2, 3, 4), Col(4, 3, 4))),
    (1, Seq(Col(1, 3, 4), Col(3, 3, 4), Col(5, 3, 4)))
  ).foreach { case (i, should) =>
    test(s"cols$i") {
      assert(cols(image23)(i) === should)
    }
  }

  val imagTypeTestdata = List(
    ("PNG", "a.png"),
    ("PNG", "x/b.png"),
    ("PNG", "all.Png"),
    ("PNG", "a/a.PNG"),
    ("JPG", "a/a.jpg"),
    ("JPG", "a/a.jpeg"),
    ("JPG", "a/a.JPEG"),
    ("JPG", "a/a.JPG")
  )
  imagTypeTestdata.foreach { case (should, is) =>
    test(s"imageType$should$is") {
      assert(imageType(new File(is)) === should)
    }

  }


}