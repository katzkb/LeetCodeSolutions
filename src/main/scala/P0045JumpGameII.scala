object Solution {
  def jump(nums: Array[Int]): Int = {
    @scala.annotation.tailrec
    def f(remain: Vector[Int], position: Int, nPos: Int, cnt: Int): Int = {
      remain match {
        case head +: tail =>
          val nextPos = Math.max(nPos, head + nums(head))
          if (position == head) {
            f(tail, nextPos, nextPos, cnt + 1)
          } else {
            f(tail, position, nextPos, cnt)
          }
        case _ => cnt
      }
    }
    f((0 until nums.length - 1).toVector, 0, 0, 0)
  }
}
// Status:  Accepted
// Runtime: 596 ms
// Memory:  54.1 MB
