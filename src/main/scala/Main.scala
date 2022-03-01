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


  val foryield =
    (for (c <- 'a' to 'e')
      yield for (n <- 1 to 5)
        yield c -> n
      ).flatten
  foryield.foreach(println)
  println("_" * 20)

  val thesame =
    ('a' to 'e').map { c =>
      (0 to 5).map { n =>
        c -> n
      }
    }
  thesame.foreach(println)

  val anotherway = for {c <- 'a' to 'c'
                        n <- 0 to 7 if n % 2 == 0
                        } yield c -> n
  anotherway.foreach(println)

  def fibonacci(n: Int): Int =
    if (n <= 1) 1 else fibonacci(n - 1) * n

  1 to 9 map fibonacci foreach println

  println("_" * 20)

  def fibonacciTail(n: Int): Int = {
    @scala.annotation.tailrec
    def loop(x: Int, acc: Int): Int =
      if (x <= 1) acc else loop(x = x - 1, acc = acc * x)

    loop(x = n, acc = 1)
  }

  1 to 12 map fibonacci foreach println

  val listaa = List(1, 2, 3)
  println(listaa :+ 0)
  println(0 :: listaa)
  println(listaa ::: List(4, 5, 6))

  object MyCollection {
    def apply(ints: Int*): Seq[Int] =
      ints
  }

  val collection = MyCollection(1, 2, 3)
  println(collection)

  println(
    List(1, 2, 3, 4).foldLeft(1) { (acc, current) =>
      acc * current
    }
  )
  println(
    List(1, 2, 3, 4).foldLeft(0) {
      _ + _
    }
  )
  println("_" * 20)

  object Calculator {
    var calculatorsCreated: Int = 0
  }

  class Calculator(a: Int) {
    println("executed during the construction")
    Calculator.calculatorsCreated += 1

    def add(b: Int): Int = a + b
  }

  val c = new Calculator(2)
  val c1 = new Calculator(2)
  val c2 = new Calculator(2)
  println("Calculators created: " + Calculator.calculatorsCreated)
  val result2 = c.add(3)
  println(result2)
  println("_" * 20)

  class Calculator2(a: Int) extends SubtractWithAdd {
    def add(b: Int): Int = a + b
  }

  trait Add {
    def add(b: Int): Int
  }

  trait SubtractWithAdd extends Add {
    def subtract(b: Int): Int = add(-b)
  }

  val c3 = new Calculator2(1)
  println(c3.add(4))
  println(c3.subtract(4))
  println("_" * 20)

  println("more typical")

  object Calculator3 extends Add2 with Subtract2

  trait Add2 {
    def add(a: Int, b: Int): Int = a + b
  }

  trait Subtract2 {
    def subtract(a: Int, b: Int): Int = a - b
  }

  println(Calculator3.add(1, 2))
  println("_" * 10 + " objects " + "_" * 10)

  class Person(name: String, age: Int)

  val p1 = new Person("Bob", age = 27)
  val p2 = new Person("Bob", age = 27)
  println(p1 == p2)

  println("_" * 10 + " data structure " + "_" * 10)

  case class Person2(name: String, age: Int)

  val p3 = Person2("Bob", age = 27)
  val p4 = Person2("Bob", age = 27)
  println(p3 == p4)

  val Person2(n, a) = p3
  println(n + " - " + a.toString)
  println(Set(p3, p4))

  val p5 =
    p3.copy(age = 33, name = "John")
  println(p5)

  //  p3.age = 25 //  VALID IF AGE IS VAR
  // a class can only extend one abstract class
  // but many traits as it wants
  // traits cannot have a constructor
  println("_" * 30)

  sealed abstract class Entity

  object Entity {
    final case class Person3(name: String, age: Int) extends Entity

    final case class Employee(id: Int, person: Person3) extends Entity

    case object Unidentified extends Entity
  }

  val p: Entity =
    Entity.Person3(
      name = "Alice",
      age = 25
    )
  val e: Entity =
    Entity.Employee(
      id = 122,
      person = Entity.Person3(
        name = "Bob",
        age = 28
      )
    )

  println(p)
  println(e)

  println("_" * 30)

  def shouldPaySalary(e: Entity): Boolean = e match {
    case _: Entity.Employee => true
    case _ => false
  }

  println(shouldPaySalary(p))
  println(shouldPaySalary(e))

  println("_" * 30)

  def addOneAndTurnIntoString(option: Option[Int]): Option[String] =
    option match {
      case Some(n) => Some((n + 1).toString)
      case None => None
    }

  val option1: Option[Int] = Some(123)

  println(addOneAndTurnIntoString(option1))

  val option2: Option[Int] = None

  println(addOneAndTurnIntoString(option2))

  println("_" * 30)

  val people: List[Entity.Person3] =
    List(
      Entity.Person3("Alice", 25),
      Entity.Person3("Bobby", 26)
    )
  println(people.find(_.name.toLowerCase.startsWith("g")))
  println(people.find(_.name.toLowerCase.startsWith("bob")))
  println(people.find(_.age % 2 != 0))

  println("_" * 30)

  @scala.annotation.tailrec
  def age(e: Entity): Option[Int] = e match {
    case Entity.Employee(_, person) => age(person)
    case Entity.Person3(_, result) => Some(result)
    case Entity.Unidentified => None
  }

  println(age(e))
  println(age(p))
  println(age(Entity.Unidentified))

  println("_" * 30)

  val sumOfAllAges: Option[Int] =
    for {
      ageE <- age(e)
      ageP <- age(p)
    } yield ageE + ageP

  val averAge: Option[Double] =
    sumOfAllAges.map(_.toDouble).map(_ / 2)

  println(sumOfAllAges)
  sumOfAllAges.foreach(println)
  averAge.foreach(println)
  println("_" * 30)

  final case class Apple(weightInGrams: Int, color: String)

  final case class Orange(weightInGrams: Int)

  def function(orange: Orange): Unit = {
    println(orange)
  }

  //  implicit def AppleWrapper(apple: Apple): AppleWrapper =
  //    new AppleWrapper(apple)

  final implicit class AppleWrapper(private val apple: Apple) extends AnyVal {
    def toOrange: Orange = Orange(apple.weightInGrams)
  }

  implicit def appleCanBeUsedAsOrange(apple: Apple): Orange = {
    Orange(apple.weightInGrams)
  }

  println("ohh la wea la zorra ctm")
  function(
    Apple(
      weightInGrams = 300,
      color = "Green"
    ).toOrange)
  println("_" * 30)

  final implicit class AnyOps(private val self: Any) extends AnyVal {
    def styled(style: String): String = style + self + Console.RESET

    def red: String = styled(Console.RED)

    def blue: String = styled(Console.BLUE)
  }

  println(List(1, 2, 3).red)
  println(List(1, 2, 3).blue)
  println(List(1, 2, 3).styled(Console.GREEN))
  println(List(1, 2, 3).styled("not safe"))
}
