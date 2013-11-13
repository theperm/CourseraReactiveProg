package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.collection.generic.Sorted
import scala.collection.generic.Sorted

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //  If you insert any two elements into an empty heap, 
  //  finding the minimum of the resulting heap should get 
  //  the smallest of the two elements back.
  property("insert 2 elements into empty heap returns smallest") = forAll {
    (x: Int, y: Int) =>
      {
        val h = insert(y, insert(x, empty))
        findMin(h) == List(x, y).min
      }
  }
  //  If you insert an element into an empty heap, 
  //  then delete the minimum, the resulting heap should be empty.
  property("insert into empty and delete should return empty heap") = forAll {
    (x: Int) => isEmpty(deleteMin(insert(x, empty)))
  }

  //  Given any heap, you should get a sorted sequence of elements 
  //  when continually finding and deleting minima. 
  //  (Hint: recursion and helper functions are your friends.)
  property("sorted seq of elements when findMin and delete minima") = forAll {
    (a: Int, b: Int, c: Int) =>
      findMinAndDeleteMin(insert(a, insert(b, insert(c, empty)))) == List(a, b, c).sorted
  }
  //  Finding a minimum of the melding of any two heaps should 
  //  return a minimum of one or the other.
  property("findMin of two melded heaps returns smallest of one or the other") = forAll {
    (x: Int, y: Int) =>
      {
        val h = meld(insert(x, empty), insert(y, empty))
        findMin(h) == List(x, y).min
      }
  }

  def findMinAndDeleteMin(h: H): List[Int] = {
    def toList(l: List[Int], heap: H): List[Int] =
      if (isEmpty(heap)) l.reverse
      else toList(findMin(heap) :: l, deleteMin(heap))
    toList(Nil, h)
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
