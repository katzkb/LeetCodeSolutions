object Solution {
  def convert(s: String, numRows: Int): String = {
    @scala.annotation.tailrec
    def f(remain: Seq[String], res: Map[Int, Seq[String]] = Map.empty, idx: Int = 0, countUp: Boolean = true): String = {
      remain match {
        case h +: tail =>
          val cntUp = (idx == 0) || ((idx != numRows - 1) && countUp)
          val newRes = res + (idx -> (res.get(idx).toSeq.flatten :+ h))
          val newIdx = if (cntUp) idx + 1 else idx - 1
          f(tail, newRes, newIdx, cntUp)
        case _ =>
          res.toSeq.sortBy(_._1).flatMap(_._2).mkString("")
      }
    }
    f(s.split(""))
  }
}
// Status:  Accepted
// Runtime: 880 ms
// Memory:  55.5 MB
