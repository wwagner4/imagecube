package imagecube1

import org.scalatest.FunSuite

class ImagecubeSuite extends FunSuite {

  val cutParamsTestValues = List(
    (7, 6, CutParams(Range(0, 1), Range(2, 3), Range(4, 5), Range(0, 1), Range(2, 3), Range(4, 5))),
    (9, 7, CutParams(Range(1, 2), Range(3, 4), Range(5, 6), Range(0, 1), Range(2, 3), Range(4, 5))),
    (6, 10, CutParams(Range(0, 1), Range(2, 3), Range(4, 5), Range(2, 3), Range(4, 5), Range(6, 7))),
    (12, 10, CutParams(Range(1, 3), Range(4, 6), Range(7, 9), Range(0, 2), Range(3, 5), Range(6, 8)))
  )

  cutParamsTestValues.foreach {
    case (w, h, p) => test(f"cutParams-$w%03d-$h%03d") {
      assert(ImagecubeUtil.cutParams(w, h) === p)
    }
  }

  val transposeParamsLeftTestValues = Seq(
    (5, 6, CutParams(Range(0, 0), Range(1, 1), Range(2, 2), Range(1, 1), Range(2, 2), Range(3, 3)), (Range(3, 1), Range(0, 0))),
    (5, 7, CutParams(Range(0, 0), Range(1, 1), Range(2, 2), Range(2, 2), Range(3, 3), Range(4, 4)), (Range(4, 2), Range(0, 0))),
    (5, 8, CutParams(Range(0, 0), Range(1, 1), Range(2, 2), Range(2, 2), Range(3, 3), Range(4, 4)), (Range(4, 2), Range(0, 0))),
    (6, 6, CutParams(Range(0, 1), Range(2, 3), Range(4, 5), Range(0, 1), Range(2, 3), Range(4, 5)), (Range(5, 0), Range(0, 1))),
    (7, 6, CutParams(Range(0, 1), Range(2, 3), Range(4, 5), Range(0, 1), Range(2, 3), Range(4, 5)), (Range(5, 0), Range(0, 1))),
    (8, 6, CutParams(Range(1, 2), Range(3, 4), Range(5, 6), Range(0, 1), Range(2, 3), Range(4, 5)), (Range(5, 0), Range(1, 2)))
  )
  transposeParamsLeftTestValues.foreach {
    case (w, h, p, should) => test(f"transposeParamsLeftTestValues-$w%03d-$h%03d") {
      assert(ImagecubeUtil.transposeParamsLeft(p) === should)
    }
  }

  val transposeParamsRightTestValues = Seq(
    (5, 6, CutParams(Range(0, 0), Range(1, 1), Range(2, 2), Range(1, 1), Range(2, 2), Range(3, 3)), (Range(3, 1), Range(2, 2))),
    (5, 7, CutParams(Range(0, 0), Range(1, 1), Range(2, 2), Range(2, 2), Range(3, 3), Range(4, 4)), (Range(4, 2), Range(2, 2))),
    (5, 8, CutParams(Range(0, 0), Range(1, 1), Range(2, 2), Range(2, 2), Range(3, 3), Range(4, 4)), (Range(4, 2), Range(2, 2))),
    (6, 6, CutParams(Range(0, 1), Range(2, 3), Range(4, 5), Range(0, 1), Range(2, 3), Range(4, 5)), (Range(5, 0), Range(4, 5))),
    (7, 6, CutParams(Range(0, 1), Range(2, 3), Range(4, 5), Range(0, 1), Range(2, 3), Range(4, 5)), (Range(5, 0), Range(4, 5))),
    (8, 6, CutParams(Range(1, 2), Range(3, 4), Range(5, 6), Range(0, 1), Range(2, 3), Range(4, 5)), (Range(5, 0), Range(5, 6)))
  )
  transposeParamsRightTestValues.foreach {
    case (w, h, p, should) => test(f"transposeParamsRightTestValues-$w%03d-$h%03d") {
      assert(ImagecubeUtil.transposeParamsRight(p) === should)
    }
  }
}
