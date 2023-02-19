object List2Exercise {

  //Zadanie 2
  def fib(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1
    else return fib(n - 1) + fib(n - 2)
  }

  def fibTail(n: Int): Int = {
    def fibWithLastValue(n: Int, previous: Int, beforePrevious: Int): Int = {
      if (n == 1) previous
      else fibWithLastValue(n - 1, previous + beforePrevious, previous)
    }

    if (n == 0) 0
    else if (n == 1) 1
    else fibWithLastValue(n, 1, 0)
  }

  def root3(a: Double, e: Double): Double = {
    def root3In(a: Double, e: Double, x: Double): Double = {
      val left = (x * x * x - a)/a
      if (left >= 0 && left <= e) x
      else if(left < 0 && -left <= e) x
      else root3In(a, e, x + ((a / (x * x)) - x) / 3)
    }

    if (a == 0) 0
    if (e <= 0) 0
    if (a > 1) root3In(a, e, a / 3)
    else root3In(a, e, a)
  }

  def power(x: Double, n: Int): Double = {
    def powerIn(x: Double, n: Int, y: Double): Double = {
      if (n == 1) 1 / x
      else powerIn(x * y, n - 1, y)
    }

    powerIn(x, n, x)
  }

  //Zadanie 4
  def exercise4a[A](list: List[A]): Unit = {
    val List(_, _, x, _, _) = list
    println(x)
  }

  def exercise4b[A](list: List[A]): Unit = {
    val List(_, (x, _)) = list
    println(x)
  }

  def initSegment[A](pref : List[A], list : List[A]): Boolean = {
    pref match {
      case Nil => true
      case h::t =>
        list match {
          case Nil => false
          case h2 :: t2 =>
            if h == h2 then initSegment[A](t, t2)
            else false
        }
    }
  }

  def replaceNth[A](list : List[A], index : Int, newElement : A): List[A] = {
    def replaceNthIn [A](list : List[A], index : Int, newElement : A, newList : List[A]): List[A] = {
        if(index < 0) newList
        list match {
          case Nil => list
          case h::t =>
            if index == 0 then (newList):::List(newElement):::t
            else replaceNthIn[A](t, (index-1), newElement, newList:::List(h))
        }
    }
    val newList : List[A] = Nil
    return replaceNthIn[A](list, index, newElement, newList)
  }

  @main def main() = {
  //      println("Zwykłe fib: " + fib(42)) // 2 minuty plus minus
        println("fibTail: " + fibTail(42))

  //println("Pierwiastek stopnia 3 : " + root3(14.0, power(10, 15)))

  //  exercise4a(List(-2, -1, 0, -1, -2))
  //  exercise4b(List((1, 2), (0, 1)))

    println(initSegment[Int](List(1,2,3,4), List(1,2,3,4,5,6,7,8,9)))
    println(replaceNth[Int](List(1,2,3,4), 3, 99))
  }
}