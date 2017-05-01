package imagecube1

import org.scalatest.FunSuite

class ImagecubeSuite extends FunSuite {

  import Imagecube._

  val cutParamsTestValues = List(
    (7, 6, CutParams(Range(0, 1), Range(2, 3), Range(4, 5), Range(0, 1), Range(2, 3), Range(4, 5))),
    (9, 7, CutParams(Range(1, 2), Range(3, 4), Range(5, 6), Range(0, 1), Range(2, 3), Range(4, 5))),
    (6, 10, CutParams(Range(0, 1), Range(2, 3), Range(4, 5), Range(2, 3), Range(4, 5), Range(6, 7))),
    (12, 10, CutParams(Range(1, 3), Range(4, 6), Range(7, 9), Range(0, 2), Range(3, 5), Range(6, 8)))
  )

  cutParamsTestValues.foreach {
    case (w, h, p) => test(s"cutParams$w$h") {
      assert(cutParams(w, h) === p)
    }
  }


}
