object Solution {
  def getPermutation(n: Int, k: Int): String = {
    val numVec = (1 to n).toVector
    val otherDigits = numVec.foldLeft((numVec, "")){ case ((remain, res), _) =>
      val remainLength = Math.max(remain.length - 1, 0)
      val digitVec     = (0 to remainLength).toVector
      val digitSize    = digitVec.tail.product
      val digitStr     = digitVec.map(_.toString * digitSize).mkString
      val dKey         = (k-1)%digitStr.length
      val numKey       = digitStr(dKey).toString.toInt
      val digit        = remain(Math.min(numKey, remainLength))
      (remain.filterNot(_ == digit), res + digit)
    }
    otherDigits._2
  }
}
// Status:  Accepted
// Runtime: 704 ms
// Memory:  57.8 MB
