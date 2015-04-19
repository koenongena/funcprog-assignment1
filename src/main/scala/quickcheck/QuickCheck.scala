package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(genHeap, const(empty))
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
/*
If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
 */
  property("hint1") = forAll { (a: Int, b: Int) =>
        val h2 = insert(a, insert(b, empty))
        findMin(h2) == (if (a < b) a else b)
  }

  /*
If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
   */
  property("hint2") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  /*Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
   */

  property("hint3") = forAll { h: H =>
    def recDeletion(heap: H, acc: List[Int]): List[Int] = {
      if (isEmpty(heap)) acc
      else findMin(heap) :: recDeletion(deleteMin(heap), acc)
    }
    val deleted: List[Int] = recDeletion(h, Nil)
    deleted == deleted.sorted
  }
  /*

Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
   */

  property("hint4") = forAll { (h1: H, h2:H) =>
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)
    val min: A = if (h1Min <= h2Min) h1Min else h2Min

    min == findMin(meld(h1, h2))
  }
}
