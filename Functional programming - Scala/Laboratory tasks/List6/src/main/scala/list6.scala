import java.util

object list6 {

  //Zadanie 1
  def stirling(n : Int, m : Int): Int = {
    if(n <= 0 || m <= 0) 0
    else if(n == m) 1
    else if(m == 1) 1
    else stirling(n-1,m-1) + m*stirling(n-1,m)
  }

  def memoizedStirling(n : Int, m : Int, hashTable : util.Hashtable[(Int, Int), Int]): Int = {
    if (n <= 0 || m <= 0) 0
    else if (n == m) 1
    else if (m == 1) 1
    else
      if(hashTable.contains((n,m))) hashTable.get((n,m))
      else{
        val newValue = memoizedStirling(n-1, m-1, hashTable) + m*memoizedStirling(n-1, m, hashTable)
        hashTable.put((n,m), newValue)
        newValue
      }
  }

  //Zadanie 2
  def fib(n : Int): Int = {
    if(n == 0) 0
    else if(n == 1) 1
    else fib(n-1)+fib(n-2)
  }

  def make_memoize[A,B](theFunction: A=>B): (A=>B) = {
    val hashTable = util.Hashtable[A,B]
    def theFunctionButBetter(argument : A): B = {
      if(hashTable.contains(argument)) hashTable.get(argument)
      else {
        val result = theFunction(argument)
        hashTable.put(argument, result)
        result
      }
    }
    theFunctionButBetter
  }


  @main def main(): Unit = {
    println(stirling(10,9))
    val hash = util.Hashtable[(Int, Int), Int]
    println(memoizedStirling(10,9, hash))

    println(make_memoize[Int, Int](fib))
  }
}
