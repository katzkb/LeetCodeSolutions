object Solution {
  def fullJustify(words: Array[String], maxWidth: Int): List[String] = {
    @scala.annotation.tailrec
    def f(
      remain: Vector[String],
      stack:  Vector[String],
      res:    Vector[String]
    ): Vector[String] = {
      val stackWordNum    = stack.length
      val stackWordLength = stack.map(_.length).sum
      remain match {
        case head +: tail
          if (stackWordLength + Math.max(stackWordNum-1, 0) + head.length < maxWidth) || stack.isEmpty && head.length == maxWidth =>
          f(tail, stack :+ head, res)
        case head +: tail =>
          val spaceNum = Math.max(maxWidth - stackWordLength, 0)
          val spaceStr = " " * spaceNum
          val unit = spaceNum / Math.max(stackWordNum-1, 1)
          val rm = spaceNum % Math.max(stackWordNum-1, 1)
          val x = Math.max(Math.max(rm, unit)/Math.max(spaceNum, 1), 1)
          val text = stack.zipWithIndex.foldLeft(""){ case (joined, (word, index)) =>
            if (rm != 0 && index < rm) {
              joined + word + " " * (x + unit)
            } else {
              (joined + word + spaceStr.slice(unit * index, unit * (index + 1))).take(maxWidth)
            }
          }
          f(head +: tail, Vector.empty, res :+ text)
        case _ => {
          val separated = stack.mkString(" ")
          val diff = maxWidth - separated.length
          res :+ (separated + (" " * diff))
        }
      }
    }
    f(words.toVector, Vector.empty, Vector.empty).toList
  }
}
// Status:  Accepted
// Runtime: 484 ms
// Memory:  51.6 MB
