object Solution {
  def solveNQueens(n: Int): List[List[String]] = {
    val rst = scala.collection.mutable.ListBuffer[List[String]]()
    dfs(n, List[Int](), rst)
    rst.toList
  }

  def dfs(n: Int, oneSol: List[Int], rst: scala.collection.mutable.ListBuffer[List[String]]): Unit = {
    if (oneSol.length == n) {
      val line = for (idx <- oneSol) yield ("."*idx+"Q"+"."*(n-idx-1))
      rst += line
      return
    }
    for (col <- 0 until n) {
      if (!oneSol.contains(col)) {
        var i = oneSol.length - 1
        var j = col - 1
        var k = col + 1
        var continue = true
        while (continue && i >= 0 && (j >= 0 || k < n)) {
          if (oneSol(i) == j || oneSol(i) == k) {
            continue = false
          }
          i -= 1
          j -= 1
          k += 1
        }
        if (continue) {
          dfs(n, oneSol:+col, rst)
        }
      }
    }
  }
}
// Status:  Accepted
// Runtime: 484 ms
// Memory:  55.1 MB
