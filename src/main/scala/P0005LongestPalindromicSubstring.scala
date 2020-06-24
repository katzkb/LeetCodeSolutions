object Solution {
  def longestPalindrome(s: String): String = {
    val size = s.length
    @scala.annotation.tailrec
    def f(cnt: Int): String = {
      val diff = size - cnt
      val strSeq = for { i <- 0 to diff } yield s.slice(i, cnt + i)
      strSeq.find(v => v == v.reverse) match {
        case Some(sv) => sv
        case None     => f(cnt - 1)
      }
    }
    f(size)
  }
}
//Status: Accepted
//Runtime: 3660 ms
//Memory: 58.1 MB
