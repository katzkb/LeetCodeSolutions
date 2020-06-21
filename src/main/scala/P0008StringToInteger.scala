object Solution {
  def myAtoi(str: String): Int = {
    import scala.util.Try

    val p = "(\\s*)([-|+][0-9]+|[0-9]*)(.*)".r
    val (min: Int, max: Int) = (Int.MinValue, Int.MaxValue)

    def getStr(str: String): String =
      str match {
        case p(_, v, _) if v.nonEmpty => v
        case _                       => "0"
      }

    def toIntOpt(str: String): Option[Int] =
      Try(str.toInt).toOption

    val s = getStr(str)
    (toIntOpt(s), s.head.toString) match {
      case (Some(v), _) => v
      case (None,  "-") => min
      case            _ => max
    }
  }
}
//Status: Accepted
//Runtime: 552 ms
//Memory: 51 MB
