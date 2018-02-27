package frl.driesprong


object PizzaExample extends App {

  import java.io.File
  import java.io.PrintWriter
  import scala.annotation.tailrec
  import scala.io.Source
  import scala.util.Random

  val dataset = "big"

  val rand = new Random()

  val lines = Source.fromFile(s"/Users/fokkodriesprong/Downloads/$dataset.in").getLines.toArray

  val header = lines.head.split(' ')

  val pizzaRows           = header(0).toInt
  val pizzaCols           = header(1).toInt
  val ingredientsPerSlice = header(2).toInt
  val maxCellsPerSlice    = header(3).toInt

  println(s"Ingredients: $ingredientsPerSlice")
  println(s"Max cells: $maxCellsPerSlice")
  println(s"Rows: $pizzaRows")
  println(s"Cols: $pizzaCols")

  val pizza = lines.tail.map(_.toCharArray)

  var globalMax = 0

  def checkIfNewRecord(slices: Set[PizzaSlice]): Set[PizzaSlice] = {
    val newMax = slices.toList.map(_.size).sum

    this.synchronized {
      // Vind de beste tot nu toe
      if (newMax > globalMax) {
        globalMax = newMax

        println(s"$globalMax out of ${pizzaRows * pizzaCols}: ${
          Math
            .round((globalMax.toDouble / (pizzaRows * pizzaCols).toDouble) * 100.0)
        }%")

        val output = s"${slices.size}\n" +
          slices
            .map(wonSlice => s"${wonSlice.x1} ${wonSlice.y1} ${wonSlice.x2} ${wonSlice.y2}")
            .mkString("\n")

        val fileTemp = new File(s"/Users/fokkodriesprong/Desktop/hashcode/$dataset.out")
        if (fileTemp.exists) {
          fileTemp.delete()
        }

        val pw = new PrintWriter(fileTemp)
        pw.write(output)
        pw.close()
      }
    }

    slices
  }

  val getAllCoordinates = {
    val coordinates = for {
      x <- 0 until pizzaRows
      y <- 0 until pizzaCols
    } yield {
      (x, y)
    }
    coordinates.toSet
  }

  @tailrec
  def growPizza(sls: Set[PizzaSlice], stepsLeft: Int = maxCellsPerSlice): Set[PizzaSlice] =
    if (stepsLeft == 0) {
      sls.filter(_.size <= maxCellsPerSlice)
    } else {
      growPizza(sls ++ sls.par.flatMap(_.growInAllDirections), stepsLeft - 1)
    }

  println("Getting all the possible slices")

  val tot = pizzaRows * pizzaCols
  var pos = 0


  val validSquares = getAllCoordinates
    .par
    .foldLeft(Set[PizzaSlice]())((a, p) => {
      val s = growPizza(Set(PizzaSlice(p._1, p._1, p._2, p._2)))

      pos += 1
      println(s"($pos/$tot) ${Math.round((pos.toDouble / tot.toDouble) * 100)}%")

      a ++ s
    })
    .filter(r => r.x1 >= 0 && r.x2 < pizzaRows && r.y1 >= 0 && r.y2 < pizzaCols)
    .filter(_.getIngredientsPerSlice(pizza) >= ingredientsPerSlice)

  println(s"Found $validSquares valid slices!!1!11")

  // The biggest carry the most value
  val sortedValidSquared = validSquares.toIndexedSeq.sortBy(_.size * -1)

  @tailrec
  def fitSquares(list: IndexedSeq[PizzaSlice], currentResult: Set[PizzaSlice]): Unit = if (list.nonEmpty) {
    val remaining = list.dropWhile(_.sliceIntersectsWith(currentResult))
    remaining.headOption match {
      case Some(slice: PizzaSlice) => fitSquares(remaining.tail, checkIfNewRecord(currentResult + slice))
      case None => println("Exhausted the greedy search")
    }
  }

  println("Trying to optimize the best cut of the pizza")
  sortedValidSquared.par.foreach(
    nextSlice => fitSquares(sortedValidSquared, Set(nextSlice))
  )
}
