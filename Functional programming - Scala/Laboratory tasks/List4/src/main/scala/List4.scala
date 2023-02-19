import scala.collection.View.Filter

object List4 {

   //Zadanie 1
   def map[A](list : List[A], f : A => A): List[A] = {
      def mapIn[A](list: List[A], newList: List[A], f: A=>A): List[A] ={
         list match {
            case Nil => newList
            case h :: t => mapIn[A](t, newList:::List(f(h)), f)
         }
      }
      mapIn[A](list, Nil, f)
   }

   //Zadanie 2
   def filter[A](list: List[A], f: A => Boolean): List[A] = {
      def filterIn[A](list : List[A], newList : List[A], f: A => Boolean): List[A] = {
         list match {
            case Nil => newList
            case h :: t =>
               f(h) match {
                  case false => filterIn[A](t, newList, f)
                  case true => filterIn[A](t, newList:::List(h), f)
               }
         }
      }
      filterIn[A](list, Nil, f)
   }

   //Zadanie 3
   def reduce[A](list : List[A], acc : A, f: (A, A) => A): A = {
      list match {
         case Nil => acc
         case h::t => reduce[A](t, f(acc, h), f)
      }
   }

   def sqr(x : Int): Int = {x*x}
   def smallerThen3(x : Int): Boolean = {
      if x < 3 then true
      else false
   }
   def sum(x : Int, y : Int): Int = {x+y}

   //Zadanie 4
   def mean(list: List[Int]): Float = {
      ( (list foldLeft 0)((x, y) => x + y) ) / ((list foldLeft 0)((x, y) => x + 1))
   }

   //Zadanie 5
   def akronym(str : String): String = {
      var wasSpace = true
      def isThisFirstLetterOfWord(c : Char): Boolean = {
        c match {
           case ' ' =>
              wasSpace = true
              return false
           case _ =>
              if(wasSpace){
                 wasSpace = false
                 return true
              }
              else false
        }
      }
      str.filter(isThisFirstLetterOfWord)
   }

   //Zadanie 6
   def sqrSmallerThenX3(list: List[Int]): List[Int] = {
      val sum = (list foldLeft 0)((x, y) => x + y)
      def isCubeSmallerThenSum(x : Int): Boolean = {
         if x*x*x <= sum then true
         else false
      }
      filter[Int](list, isCubeSmallerThenSum)
   }

   @main def main(): Unit = {
      println(map[Int](List(1, 2, 3, 4, 5), sqr))
      println(filter[Int](List(1, 2, 3, 4, 5), smallerThen3))
      println(reduce[Int](List(1, 2, 3, 4, 5), 0, sum))
      println(mean(List(1, 2, 3, 4, 5)))
      println(akronym("Zakład Ubezpieczeń Społecznych"))
      println(sqrSmallerThenX3(List(1, 2, 3, 4, 5)))
   }
}
