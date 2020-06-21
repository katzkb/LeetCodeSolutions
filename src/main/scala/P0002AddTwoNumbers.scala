/**
 * Definition for singly-linked list.
 * class ListNode(var _x: Int = 0) {
 *   var next: ListNode = null
 *   var x: Int = _x
 * }
 */
class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}
object P0002AddTwoNumbers {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val sum = listNodeToBigInt(l1) + listNodeToBigInt(l2)
    genListNode(sum.toString.split("").toVector.map(_.toInt).reverse)
  }
  @scala.annotation.tailrec
  def listNodeToBigInt(l: ListNode, result: String = ""): BigInt = {
    val x = l.x
    val xString = x.toString
    val next = l.next
    if (next != null) {
      listNodeToBigInt(next, xString + result)
    } else {
      BigInt(xString + result)
    }
  }
  def genListNode(nums: Vector[Int]): ListNode =
    nums.init.foldRight(new ListNode(nums.last))((num, z) => {
      val l = new ListNode(num)
      l.next = z
      l
    })
}
//Status: Accepted
//Runtime: 464 ms
//Memory: 54.3 MB
