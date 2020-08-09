/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution {
  def reverseKGroup(head: ListNode, k: Int): ListNode = {
    @scala.annotation.tailrec
    def f(remain: Vector[Int], res: Vector[Int] = Vector.empty): Vector[Int] = {
      if (remain.size >= k) {
        f(remain.drop(k), res ++ remain.take(k).reverse)
      } else {
        res ++ remain
      }
    }
    genListNode(f(listNodeToVector(head)))
  }

  @scala.annotation.tailrec
  def listNodeToVector(listNode: ListNode, res: Vector[Int] = Vector.empty): Vector[Int] = {
    if (listNode == null) {
      res
    } else {
      val newRes = res :+ listNode.x
      if (listNode.next != null) {
        listNodeToVector(listNode.next, newRes)
      } else {
        res :+ listNode.x
      }
    }
  }

  def genListNode(nums: Vector[Int]): ListNode = {
    if (nums.nonEmpty) {
      nums.init.foldRight(new ListNode(nums.last))((num, z) => {
        val l = new ListNode(num)
        l.next = z
        l
      })
    } else {
      null
    }
  }
}
// Status:  Accepted
// Runtime: 584 ms
// Memory:  51.7 MB
