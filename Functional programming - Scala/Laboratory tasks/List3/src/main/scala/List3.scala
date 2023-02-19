import scala.Null
import scala.runtime.Nothing$

object List3 {

  //Zadanie 1
  def lastElement(list: List[Any]): Any = {
    def isLongerThan1(list: List[Any]): Int = {
      if (list == Nil) return 0 // Zero means this is empty list
      val h :: t = list
      if (t == Nil) return 1 // One means this list has only one element
      else return 2 // Two means in this list is 2 or more elements
    }
    val length = isLongerThan1(list)
    if(length == 0) return None
    val h::t = list
    if(length == 1) return h
    return lastElement(t)
  }

  //Zadanie 2
  def twoLastElements(list: List[Any]): List[Any] = {
    def isLongerThan2(list: List[Any]): Int = {
      if (list == Nil) return 0 // Zero means this is empty list
      val h1 :: t1 = list
      val h2 :: t2 = t1
      if (t2 == Nil) return 1 // One means this list has only two elements
      else return 2 // Two means in this list is 2 or more elements
    }
    val length = isLongerThan2(list)
    if(length == 0) return Nil
    val h::t = list
    if (length == 1) return list
    else return twoLastElements(t)
  }

  //Zadanie 3
  def length[A](list: List[A]): Int = {
    def hiddenLength[A](list: List[A], length: Int): Int = {
      if (list == Nil) return length;
      val h :: t = list
      return hiddenLength[A](t, length + 1)
    }
    return hiddenLength[A](list, 0)
  }


  //Zadanie 4
  def reverse[A](list: List[A]): List[A] = {
    if(list == Nil) Nil
    val h::t = list
    if(t == Nil) return List(h)
    return (reverse[A](t)) ::: List(h)
  }

  //Zadanie 5
  def palindrom(string: String): Boolean = {
    def checkIsTheseListsAreEqual[A](firstList: List[A], secondList: List[A]): Boolean = {
      val h1 :: t1 = firstList
      val h2 :: t2 = secondList
      if (t1 == Nil || t2 == Nil) return (h1 == h2)
      return (h1 == h2 && checkIsTheseListsAreEqual[A](t1, t2))
    }
    val list : List[Char] = string.toList
    val reverseList = reverse[Char](list)
    return checkIsTheseListsAreEqual[Char](list, reverseList)
  }

  //Zadanie 6
  def listWithoutRepetition[A](list: List[A]): List[A] = {
    def listWithUniqueElements[A](list: List[A], noRep: List[A]): List[A] = {
      if (list == Nil) return noRep
      val h :: t = list
      if (checkIsWasThisOne[A](h, noRep)) return listWithUniqueElements[A](t, noRep)
      else return listWithUniqueElements[A](t, noRep ::: List(h))
    }

    def checkIsWasThisOne[A](x: A, list: List[A]): Boolean = {
      if (list == Nil) return false
      val h :: t = list
      if (h == x) return true
      else return checkIsWasThisOne[A](x, t)
    }

    if (list == Nil) return Nil
    val h::t = list
    val noRep : List[A] = List(h)
    return listWithUniqueElements(t, noRep)
  }

  //Zadanie 7
  def onlyEvenIndexNumbers[A](list: List[A]): List[A] = {
    def newEven[A](newElement: A, newList: List[A], oldList: List[A], isEven: Boolean): List[A] = {
      if(isEven) {
        val h :: t = oldList
        if (t != Nil) return newEven[A](h, newList ::: List(h), t, false)
        else return newList ::: List(h)
      }
      else{
        val h :: t = oldList
        if (t != Nil) return newEven[A](h, newList, t, true)
        else return newList
      }
    }
    if (list == Nil) return list
    val h :: t = list
    newEven[A](h, List(), list, true)
  }

  //Zadanie 8
  def prime(x : Int): Boolean = {
    def isDivided(x: Int, n: Int): Boolean = {
      if (x % n == 0) return false
      else {
        if (n * n >= x) return true
        else return isDivided(x, n + 1)
      }
    }
    if(x < 2) return false
    if(x % 2 == 0) return true
    return isDivided(x, 2)
  }


  @main def main(): Unit = {
    //Zadanie 1
    print("Ostatni element listy: ")
    val list : List[Double] = List(1.0, 2.0, 3.0, 4.0, 5.0)
    println(lastElement(list))

    //Zadanie 2
    print("2 Ostatnie elementy listy: ")
    println(twoLastElements(list))

    println(length(list))
    println(reverse(list))

    print("Is kajak palindrom? : ")
    println(palindrom("kajak"))

    print("List with only unique elements ")
    val list2 : List[Double] = List(1.0, 2.0, 1.0, 2.0, 5.0)
    println(listWithoutRepetition(list2))

    print("Even list: ")
    println(onlyEvenIndexNumbers(list))
    print("Is 13 prime? : ")
    println(prime(13))
  }
}
