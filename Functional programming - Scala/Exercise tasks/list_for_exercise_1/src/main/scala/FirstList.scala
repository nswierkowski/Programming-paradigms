object FirstList {

  //Zadanie1
  def flattenWithoutFun[A](xss: List[List[A]]): List[A] = {
    if(xss == Nil) return Nil
    val h::t = xss
    return h:::flattenWithoutFun[A](t)
  }

  def flatten[A](xss: List[List[A]]): List[A] = {
    if (xss == Nil)  return Nil
    return xss.head ::: flatten[A](xss.tail)
  }

  //Zadanie2
  def count[A](x: A, xs: List[A]): Int = {
    if(xs == Nil) return 0
    def countIn[A](x: A, xs : List[A], sum): Int = {
      if(xs == Nil) return sum
      if(xs.head == x) countIn[A](x, xs, sum+1)
      else countIn[A](x, xs, sum)
    }

    return countIn[A](x, xs, 0)
  }

  //Zadanie3
  def replicate[A] (x: A, n: Int): List[A] = {
    if(n == 0) return Nil
    return List(x):::replicate[A](x, n-1)
  }

  //Zadanie4
  def sqrList(xs: List[Int]): List[Int]  = {
    if(xs == Nil) return Nil
    val h::t = xs
    return List(xs.head*xs.head):::sqrList(xs.tail)
  }

  //Zadanie5

//  def palindrome[A](xs: List[A]): Boolean = {
//    def areTheseListTheSame[A](first: List[A], second: List[A]): Boolean = {
//      if (first == Nil || second == Nil) return true;
//      if (first.head == second.head) return true && areTheseListTheSame(first.tail, second.tail)
//      return false
//    }
//    return areTheseListTheSame(xs, xs.reverse)
//  }

  def palindrome[A](xs: List[A]): Boolean = {
    if(xs == xs.reverse)  true
    else false
  }

  //Zadanie 6
  def listLength[A](xs : List[A]): Int = {
    if(xs == Nil)  0
    else (1 + listLength(xs.tail))
  }

  //Zadanie 7 - dla debila
  def functionD[A](n : Int): Int = {
    if (n == 1) return 1
    val power = thePower(n)
    return power + functionD(n/2)
  }

  def thePower(n: Int): Int = {
    if(n == 1) return 0
    return (1+thePower(n/2))
  }

  //Zadanie 7 - dla mądrego studenta
//  def function[A](N: Int, c : Int): Double = {
//    if (N == 1) return 1
//    val n = thePower(N)
//    return (0.5*(c*n*n + c*n+ 2))
//  }

  def function[A](N: Int, c: Int): Double = {
    def power(n: Int): Int = {
      if (n == 1) return 0
      return (1 + power(n / 2))
    }
    if(N / 2 == 1) return -1.0
    if (N == 1) return 1
    val n = thePower(N)
    return (0.5*(c*n*n + c*n+ 2))
  }

  //Lista 2
  //Zadanie 2
  def fib(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1
    else (fib(n-1) + fib(n-2))
  }

  def fibTail(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else (fib(n - 1) + fib(n - 2))
  }

  @main def main(): Unit = {
    //Zadanie 1
    val list : List[List[Double]] = List(List(1.0, 2.0), List(3.0, 4.0, 5.0))
    println(list)
    println(flatten(list))

    //Zadanie2
    val list2 : List[Char] = List('a', 'i', 'a')
    println(count('a', list2))

    //Zadanie3
    println(replicate("PARADYGMATY", 3))

    //Zadanie4
    val list3 : List[Int] = List(1, 2, 3, -4)
    println(sqrList(list3))

    //Zadanie5
    val list4: List[Int] = List(1, 2, 2, 1)
    println(palindrome(list4))
    println(palindrome(list3))

    //Zadanie6
    println(listLength(list4))

    //Zadanie7
    println(functionD(16))
    println(function(16, 1))

  }
}
