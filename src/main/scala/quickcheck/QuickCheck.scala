package quickcheck


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

  property("sorted-del") = forAll {
    h: H =>
      def deleteAll(h: H, r: List[Int]): List[Int] = if (isEmpty(h)) r else findMin(h) :: deleteAll(deleteMin(h), r)
      val dd = deleteAll(h, Nil)
      dd == dd.sorted
  }

  property("meld-two") = forAll {
    (h1: H, h2: H) =>
      def deleteAll(h: H, r: List[Int]): List[Int] = if (isEmpty(h)) r else findMin(h) :: deleteAll(deleteMin(h), r)
      val d1 = deleteAll(h1, Nil)
      val d2 = deleteAll(h2, Nil)
      val d = deleteAll(meld(h1, h2), Nil)
      (d1 ::: d2).sorted == d
  }


  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
