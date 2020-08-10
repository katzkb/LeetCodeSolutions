object Solution {
  def findSubstring(s: String, words: Array[String]): List[Int] = {
    val wordSize   = words.headOption.map(_.length).getOrElse(0)
    val wordsNum   = words.length
    val streamSize = wordSize * wordsNum
    val diff = Math.max(s.length - streamSize, 0)

    val sMap: Map[Int, String] = (for { i <- 0 to diff } yield { (i -> s.slice(i, streamSize + i))}).toMap

    if (s.isEmpty || words.isEmpty) {
      List.empty
    } else {
      sMap.filter { case (_, str) =>
        @scala.annotation.tailrec
        def f(remainS: String, remainWords: Array[String] = words): Boolean = {
          val (hit, noHit) = remainWords.partition(_ == remainS.take(wordSize))
          if (hit.nonEmpty) {
            f(remainS.drop(wordSize), hit.drop(1) ++ noHit)
          } else {
            if (remainWords.isEmpty) true else false
          }
        }
        f(str)
      }.keys.toList
    }
  }
}
// Status:  Accepted
// Runtime: 5284 ms
// Memory:  51.7 MB
