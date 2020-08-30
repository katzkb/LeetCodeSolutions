object Solution {
  def longestValidParentheses(s: String): Int = {
    // write your code in Scala 2.1
    @scala.annotation.tailrec
    def check(
      strVec: Vector[String],
      depth: Int = 0,
      cntVec :Vector[Int] = Vector(0, 0)
    ): Int =
      strVec match {
        case head +: tail if head != "" =>
          if (depth < 1 && head == ")") {
            check(tail, 0, cntVec :+ 0)
          } else if (head == "(") {
            check(tail, depth + 1, cntVec :+ 0)
          } else {
            val cntInitVec = cntVec.init
            check(tail, depth - 1, cntInitVec.init :+ cntInitVec.last + cntVec.last + 2)
          }
        case _ => cntVec.max
      }
    check(s.split("").toVector)
  }
}
// Status:  Accepted
// Runtime: 1492 ms
// Memory:  87.1 MB
