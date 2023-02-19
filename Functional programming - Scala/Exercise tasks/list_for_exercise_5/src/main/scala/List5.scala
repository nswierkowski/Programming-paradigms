object List5 {

  val lfib = {
    def lfibIn(p: Int, n: Int): Stream[Int] = {
      (p + n) #:: lfibIn(n, (p + n))
    }
    Stream.cons(0, Stream.cons(1, lfibIn(0, 1)))
  }

  @main def main(): Unit = {
    println(lfib)
  }
}
