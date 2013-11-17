package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll {
    a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("min2") = forAll {
    (a: Int, b: Int) =>
      val h = insert(a, insert(b, empty))
      findMin(h) == math.min(a, b)
  }

  property("del1") = forAll {
    a: Int =>
      val h = insert(a, empty)
      deleteMin(h) == empty
  }

  property("min-meld") = forAll {
    (h1: H, h2: H) =>
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val m = findMin(meld(h1, h2))
      m == math.min(m1, m2)
  }

  property("sorted-del") = forAll {
    h: H =>
      def deleteAll(h: H, r: List[Int]): List[Int] = if(isEmpty(h)) r else findMin(h) :: deleteAll(deleteMin(h), r)
      val dd = deleteAll(h, Nil)
      dd == dd.sorted
  }

  property("del2") = forAll {
    (a: Int, b: Int) =>
      val h = insert(a, insert(b, empty))
      deleteMin(deleteMin(h)) == empty
  }

  property("insert-same") = forAll {
    a: Int =>
      val h = insert(a, insert(a, empty))
      a == findMin(h)
      a == findMin(deleteMin(h))
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
