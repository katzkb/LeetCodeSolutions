object Solution {
  def candy(ratings: Array[Int]): Int = {
    if (ratings.length > 1) {
      val r1Vec = ratings.toVector.sliding(2)
      val r2Vec = ratings.reverse.toVector.sliding(2)
      val candy1 = r1Vec.foldLeft(Vector(1)){ case (prev, Vector(v1, v2)) => {
        val x = prev.lastOption.getOrElse(0)
        if (v1 < v2) prev :+ x + 1 else prev :+ 1
      }}
      val candy2 = r2Vec.foldLeft(Vector(1)){ case (prev, Vector(v1, v2)) => {
        val x = prev.headOption.getOrElse(0)
        if (v1 < v2) (x + 1) +: prev else 1 +: prev
      }}
      println(candy1)
      println(candy2)
      (for {
        i <- ratings.indices
      } yield Math.max(candy1(i), candy2(i))).sum
    } else 1
  }
}

// Status:  Accepted
// Runtime: 992 ms
// Memory:  62.3 MB
