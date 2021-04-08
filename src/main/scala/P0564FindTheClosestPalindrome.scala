object Solution {
  def mirroring(s: String) = {
    val x = s.substring(0, s.length / 2)
    x + (if (s.length % 2 == 1) s.charAt(s.length / 2)
    else "") + new StringBuilder(x).reverse.toString
  }

  def nearestPalindromic(n: String): String = {
    if (n == "1") return "0"
    val a = mirroring(n)
    var diff1 = Long.MaxValue
    diff1 = Math.abs(n.toLong - a.toLong)
    if (diff1 == 0) diff1 = Long.MaxValue
    var s = new StringBuilder(n)
    var i = (s.length - 1) / 2
    while ( {
      i >= 0 && s.charAt(i) == '0'
    }) {
      s.replace(i, i + 1, "9")
      i -= 1
    }
    if (i == 0 && s.charAt(i) == '1') {
      s.delete(0, 1)
      val mid = (s.length - 1) / 2
      s.replace(mid, mid + 1, "9")
    }
    else s.replace(i, i + 1, "" + (s.charAt(i) - 1).toChar)
    val b = mirroring(s.toString)
    val diff2 = Math.abs(n.toLong - b.toLong)
    s = new StringBuilder(n)
    i = (s.length - 1) / 2
    while ( {
      i >= 0 && s.charAt(i) == '9'
    }) {
      s.replace(i, i + 1, "0")
      i -= 1
    }
    if (i < 0) s.insert(0, "1")
    else s.replace(i, i + 1, "" + (s.charAt(i) + 1).toChar)
    val c = mirroring(s.toString)
    val diff3 = Math.abs(n.toLong - c.toLong)
    if (diff2 <= diff1 && diff2 <= diff3) return b
    if (diff1 <= diff3 && diff1 <= diff2) a
    else c
  }
}
// Status:  Accepted
// Runtime: 460 ms
// Memory:  51.2 MB
