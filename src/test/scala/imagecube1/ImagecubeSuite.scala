package imagecube1

import org.scalatest.FunSuite

class ImagecubeSuite extends FunSuite {

  case class Range(from: Int, to: Int)

  case class CutParams(x1: Range, x2: Range, x3: Range, y1: Range, y2: Range, y3: Range)


  def cutParams(width: Int, height: Int): CutParams = {

    def cutParamsPort(w: Int, h: Int): CutParams = {
      val h1 = (h / 3) * 3
      val off1 = (w - h1) / 2
      val step = h1 / 3
      CutParams(
        Range(off1, off1 + step - 1),
        Range(off1 + step, off1 + (2 * step) - 1),
        Range(off1 + (2 * step), off1 + (3 * step) - 1),
        Range(0, step - 1),
        Range(step, (2 * step) - 1),
        Range(2 * step, (3 * step) - 1)
      )
    }

    def transpose(p: CutParams): CutParams = {
      CutParams(
        p.y1,
        p.y2,
        p.y3,
        p.x1,
        p.x2,
        p.x3
      )
    }

    if (width >= height) cutParamsPort(width, height)
    else transpose(cutParams(height, width))
  }

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
