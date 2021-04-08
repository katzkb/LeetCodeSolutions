object Solution {
  def maximumGap(nums: Array[Int]): Int = {
    nums.toVector.sorted.sliding(2).foldLeft(0) {
      case (res, Vector(v1, v2)) =>
        if (Math.abs(v1 - v2) > res) Math.abs(v1 - v2) else res
      case _ => 0
    }
  }
}

// Status:  Accepted
// Runtime: 584 ms
// Memory:  55.1 MB
