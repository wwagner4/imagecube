package imagecube

import java.io.File

import org.scalatest._

class ImagecubeSuite extends FunSuite {

  import Imagecube._


  def image23: Image = {
    val px = Seq(1, 2, 3, 4, 5, 6)
    Image(2, 3, px)
  }

  test("pixel") {
    val p = pixel(image23)
    assert(p.size === 6)
    assert(p(0) === Pixel(0, 0, 1))
    assert(p(1) === Pixel(1, 0, 2))
    assert(p(2) === Pixel(0, 1, 3))
    assert(p(3) === Pixel(1, 1, 4))
    assert(p(4) === Pixel(0, 2, 5))
    assert(p(5) === Pixel(1, 2, 6))
  }

  Seq(
    (0, Seq(1, 2)),
    (1, Seq(3, 4)),
    (2, Seq(5, 6))
  ).foreach { case (i, should) =>
    test(s"rows$i") {
      assert(rows(image23)(i) === should)
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