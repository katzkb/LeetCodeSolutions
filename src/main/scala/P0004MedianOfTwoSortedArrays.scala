object Solution {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val unite = (nums1 ++ nums2).sorted
    val uniteSize = unite.length
    if (uniteSize % 2 == 0) {
      val (v1, v2) = (unite(uniteSize/2-1), unite(uniteSize/2))
      (v1+v2).toDouble/2
    } else {
      unite(uniteSize/2).toDouble
    }
  }
}
// Status:  Accepted
// Runtime: 660 ms
// Memory:  54.3 MB
