package imagecube1

import imagecube1.Imagecube.{colorMix, linearCompress}

case class Range(from: Int, to: Int)

case class CutParams(partLen: Int, x1: Range, x2: Range, x3: Range, y1: Range, y2: Range, y3: Range)

object ImagecubeUtil {

  def transposeParamsLeft(p: CutParams): (Range, Range) = {
    val x = Range(p.y3.to, p.y1.from)
    val y = Range(p.x1.from, p.x1.to)
    (x, y)
  }

  def transposeParamsRight(p: CutParams): (Range, Range) = {
    val x = Range(p.y3.to, p.y1.from)
    val y = Range(p.x3.from, p.x3.to)
    (x, y)
  }

  def cutParams(width: Int, height: Int): CutParams = {

    def cutParamsPort(w: Int, h: Int): CutParams = {
      val h1 = (h / 3) * 3
      val off1 = (w - h1) / 2
      val step = h1 / 3
      CutParams(
        step,
        Range(off1, off1 + step - 1),
        Range(off1 + step, off1 + (2 * step) - 1),
        Range(off1 + (2 * step), off1 + (3 * step) - 1),
        Range(0, step - 1),
        Range(step, (2 * step) - 1),
        Range(2 * step, (3 * step) - 1)
      )
    }

    def transpose(p: CutParams): CutParams = {
      CutParams(p.partLen, p.y1, p.y2, p.y3, p.x1, p.x2, p.x3)
    }

    if (width >= height) cutParamsPort(width, height)
    else transpose(cutParams(height, width))
  }

  def shortenImagePart(part: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val newRowsA = part.zipWithIndex.map { case (row, i) =>
      val n = part.size
      val (from, to) = shortenA(i, n)
      val filteredCol = row.zipWithIndex
        .filter { case (_, ir) => ir >= from && ir <= to }
        .map { case (c, _) => c }
      linearCompress(filteredCol, n / 2, colorMix)
    }
    val newRowsB = part.zipWithIndex.map { case (row, i) =>
      val n = part.size
      val (from, to) = shortenB(i, n)
      val filteredCol = row.zipWithIndex
        .filter { case (_, ir) => ir >= from && ir <= to }
        .map { case (c, _) => c }
      val m = if (n % 2 == 0) n / 2 else n / 2 + 1
      linearCompress(filteredCol, m, colorMix)
    }
    newRowsA.zip(newRowsB).map { case (a, b) => a ++ b }
  }


  def shortenA(i: Int, n: Int): (Int, Int) = {
    val a = (3.0 * n / 2.0).round.toInt
    val from = i
    val to = a
    (from, to)
  }

  def shortenB(i: Int, n: Int): (Int, Int) = {
    val a = (3.0 * n / 2.0).round.toInt
    val from = a
    val to = 2 * a - i
    (from, to)
  }



}
