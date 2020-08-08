object Solution {
  def lengthOfLongestSubstring(s: String): Int = {
    import scala.collection.immutable.ListSet
    @scala.annotation.tailrec
    def f(
      strVec:  Vector[String],
      sSet:    ListSet[String] = ListSet.empty,
      maxSize: Int = 0,
      nowSize: Int = 0
    ): Int = {
      strVec match {
        case head +: tail if head.nonEmpty =>
          // Setにすることで重複を排除
          val newSet = sSet + head
          // 追加前と後でサイズが同じなら重複したということ
          if (sSet.size == newSet.size) {
            val duplicatedStrIdx = sSet.toSeq.indexWhere(_ == head) // 重複箇所を取得
            val newSubstring     = newSet.drop(duplicatedStrIdx + 1) + head   // それ以降の文字列のみを取得
            f(tail, newSubstring, Math.max(maxSize, nowSize), newSubstring.size)
          } else {
            f(tail, newSet, maxSize, nowSize + 1)
          }
        case _ => Math.max(maxSize, nowSize)
      }
    }
    f(s.split("").toVector)
  }
}
// Status:  Accepted
// Runtime: 1428 ms
// Memory:  57 MB
