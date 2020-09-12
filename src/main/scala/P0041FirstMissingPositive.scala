object Solution {
  def firstMissingPositive(nums: Array[Int]): Int = {
    @scala.annotation.tailrec
    def f(keys: Vector[Int]): Int = {
      keys match {
        case head +: tail if nums.contains(head) => f(tail)
        case head +: _                           => head
        case _                                   => 1
      }
    }
    f((1 to nums.length + 1).toVector)
  }
}
// Status:  Accepted
// Runtime: 480 ms
// Memory:  50.7 MB
