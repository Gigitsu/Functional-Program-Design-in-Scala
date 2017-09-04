package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[A]
      h <- genHeap
    } yield insert(v, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (_: H) =>
    isEmpty(empty)
  }

  property("gen3") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val h2 = if (isEmpty(h)) insert(0, empty) else empty
    findMin(meld(h, h2)) == m
  }

  property("gen4") = forAll { (_: H) =>
    isEmpty(deleteMin(insert(0, empty)))
  }

  property("gen5") = forAll { (h: H) =>
    def foo(i: Int, h1: H): Boolean = h1 match {
      case x if isEmpty(x) => true
      case x => i <= findMin(x) && foo(findMin(x), deleteMin(x))
    }

    if (isEmpty(h)) true
    else foo(findMin(h), deleteMin(h))
  }

  property("gen6") = forAll { (_: H) =>
    val h = insert(12, insert(4, insert(2, empty)))
    findMin(h) == 2
    findMin(deleteMin(h)) == 4
    findMin(deleteMin(deleteMin(h))) == 12
  }

}
