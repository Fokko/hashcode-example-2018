package frl.driesprong

import scala.annotation.tailrec


case class PizzaSlice(x1: Int, x2: Int, y1: Int, y2: Int) {
  def getCoordinatees: Set[(Int, Int)] = {
    val coodinates = for {
      row <- x1 to x2
      col <- y1 to y2
    } yield {
      (row, col)
    }

    coodinates.toSet
  }

  def growInAllDirections: Set[PizzaSlice] = Set(
    PizzaSlice(x1 - 1, x2, y1, y2),
    PizzaSlice(x1, x2 + 1, y1, y2),
    PizzaSlice(x1, x2, y1 - 1, y2),
    PizzaSlice(x1, x2, y1, y2 + 1)
  )

  def getIngredientsPerSlice(pizza: Array[Array[Char]]): Int = {
    val ingredients = for {
      row <- x1 to x2
      col <- y1 to y2
    } yield {
      pizza(row)(col)
    }

    val groups = ingredients.groupBy(k => k)

    // Check if all the ingredients are available
    if (groups.size == 2) {
      groups.map(_._2.length).min
    } else {
      0
    }
  }

//  @tailrec
//  final def sliceIntersectsWith(slices: Set[PizzaSlice]): Boolean = slices.headOption match {
//    case Some(b: PizzaSlice) => if (x1 <= b.x2 && x2 >= b.x1 && y1 <= b.y2 && y2 >= b.y1) {
//      true
//    } else {
//      // Continue
//      sliceIntersectsWith(slices.tail)
//    }
//    case _ => false
//  }

    def sliceIntersectsWith(slices: Set[PizzaSlice]): Boolean =
      slices.par.exists(
        // true if it intersects
        b => x1 <= b.x2 && x2 >= b.x1 && y1 <= b.y2 && y2 >= b.y1
      )

  def toString(pizza: Array[Array[Char]]): String = {
    val deepPizza = pizza.map(_.clone)

    getCoordinatees.foreach(x => {
      deepPizza(x._1)(x._2) = 'X'
    })

    deepPizza.map(_.mkString).mkString("\n")
  }

  def size: Int = (1 + (x2 - x1)) * (1 + (y2 - y1))

  def canEqual(a: Any): Boolean = a.isInstanceOf[PizzaSlice]

  override def equals(that: Any): Boolean =
    that match {
      case that: PizzaSlice => that.canEqual(this) &&
        that.x1 == x1 &&
        that.x2 == x2 &&
        that.y1 == y1 &&
        that.y2 == y2
      case _ => false
    }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result *= prime + x1
    result *= prime + x2
    result *= prime + y1
    result *= prime + y2
    result
  }
}