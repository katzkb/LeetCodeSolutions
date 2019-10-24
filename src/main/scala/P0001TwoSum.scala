object P0001TwoSum {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    @scala.annotation.tailrec
    def check(remain: Vector[(Int, Int)]): Array[Int] = {
      remain match {
        case head +: tail =>
          val x = target - head._1
          tail.find(_._1 == x) match {
            case Some(y) => Array(head._2, y._2)
            case None    => check(tail)
          }
      }
    }
    check(nums.zipWithIndex.toVector)
  }
}
//Status: Accepted
//Runtime: 652 ms
//Memory: 51.3 MB
