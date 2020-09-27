object Solution {
  import scala.util.Try
  def isNumber(s: String): Boolean = {
    !s.contains("f") && !s.contains("D") && Try(s.toDouble).isSuccess
  }
}
// Status:  Accepted
// Runtime: 428 ms
// Memory:  51.1 MB
