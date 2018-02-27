package frl.driesprong

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PizzaSliceSpec extends FlatSpec with Matchers {

  "Rectangle" should "intersect with another intersecting triangle" in {
    val small = PizzaSlice(2, 2, 2, 2)
    //    TTTTT
    //    TMMMT
    //    TTXTT
    val groot = PizzaSlice(0, 2, 0, 3)
    //    XXXXT
    //    XXXXT
    //    XXXXT

    // If there is nothing, it should not intersect
    assert(small.sliceIntersectsWith(Set(groot)))
  }

  "Rectangle" should "not intersect with another intersecting triangle" in {
    // If there is nothing, it should not intersect
    assert(!PizzaSlice(0, 1, 0, 1).sliceIntersectsWith(Set()))

    assert(!PizzaSlice(0, 1, 0, 1).sliceIntersectsWith(Set(PizzaSlice(0, 1, 2, 3))))

    assert(!PizzaSlice(0, 1, 0, 4).sliceIntersectsWith(Set(PizzaSlice(2, 2, 0, 4))))
  }
}
