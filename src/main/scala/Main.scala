object Main extends App {
  println("Hello" + ' ' + "World")

  def highOrderFunction(f: Int => String): Unit = {
    val numList = List(1, 2, 3, 4, 5, 6)
    for (a <- numList) {
      println("Value of a: " + f(a))
    }

  }

  def rendered(a: Int): String =
    Console.BLUE + a + Console.RESET
  //  highOrderFunction(rendered)

  val f1: Int => String = rendered
  val f2 = rendered _
  val f3 = x => rendered(x)
  highOrderFunction(f3)

  println(
    List(3, 2, 4, 2, 0).sortWith((a, b) => a <= b)
  )
  println(
    List(3, 2, 4, 2, 0).sortWith(_ <= _)
  )
  highOrderFunction(x => Console.GREEN + x + Console.RESET)

  highOrderFunction(Console.RED + _ + Console.RESET)
  highOrderFunction {
    case 5 => Console.CYAN + 5 + Console.RESET
    case 6 => "hola"
    case x if x % 2 == 0 => Console.YELLOW + x + Console.RESET
    case x => x.toString
  }
  val result =
    List(1, 2, 3, 4, 5, 6, 7) match {
      case List(_, _, third, fourth, _*)
        if third == fourth - 1 => true
      case _ => false
    }
  println(result)

  val List(_, _, third, fourth, rest@_ *) = List(1, 2, 3, 4, 5, 6)
  println(third)
  println(fourth)
  println(rest)

  try println(1 / 0)
  catch {
    case e: ArithmeticException => print(Console.MAGENTA +
      e +
      Console.RESET)
  }
  finally println("\nno problem")

  val mutableArray = Array("asd1", "asd2", "asd")

  mutableArray(2) = "asd3"

  mutableArray.foreach(println)

  mutableArray.map(_.reverse).foreach(println)

  object MyStuff {
    val inside = 128
  }

  import MyStuff.inside

  println(inside)

  println(Iterable(1, 2, 3, 4))

  //sequences are ordered, similar than arrays but immutable
  // sets are unordered
  // maps are sets of key-values

  println("efficient access")
  println(IndexedSeq(1, 2, 3)(1))

  println("duplicates are gone")
  println(Set(1, 2, 2, 2, 4))

  println(":" * 10)
  println(scala.collection.immutable.HashSet(1, 2, 2, 2, 4))

  List(1, 2, 3)
    .filter(_ % 2 != 0)
    .map(_ + 1)
    .foreach(println)
  println("_" * 10)
  List(3, 1, 2, 3, 4, 3).flatMap { n =>
    if (n % 2 == 0)
      List.empty
    else
      List(n)
  }.foreach(println) //flatten + map

  val matrix =
    List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    )
  matrix.foreach(println)
  println(matrix.flatten)
  println("_" * 20)
  matrix.map(_.reverse).flatten.foreach(println)

  println("_" * 10)
  matrix.flatMap(_.reverse).foreach(println)

  println(matrix.map(_.reverse).flatten == matrix.flatMap(_.reverse))

  Map(1 -> "A", 2 -> "B", 3 -> "C")
    .map { x =>
      val key = x._1
      val value = x._2

      key -> value.toLowerCase

    }
    .foreach(println)
  println("_" * 20)
  Map(1 -> "A", 2 -> "B", 3 -> "C")
    .map {
      case (key, value) => key -> value.toLowerCase
    }
    .foreach(println)

  val tuple2 = List(1, 2, 3, 4, 5, 6).partition(_ % 2 == 0)
  println(tuple2)

  0 until 10 by 2 foreach println



}
