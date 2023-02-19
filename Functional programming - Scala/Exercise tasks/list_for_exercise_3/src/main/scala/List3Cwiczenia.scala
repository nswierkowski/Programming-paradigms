object List3 {
  //Zadanie 2
  def curry3[A, B, C, D](f: (A, B, C) => D) = (x: A) => (y: B) => (z: C) => f(x, y, z)

  def uncurry3[A, B, C, D](f: A => B => C => D) = (x: A, y: B, z: C) => f(x)(y)(z)


  //Zadanie 3
  def sumProd(xs: List[Int]): (Int, Int) = {
    def sumProdIn(x: (Int, Int), y: Int): (Int, Int) = {
      val (sum, product) = x
      (sum + y, product * y)
    }

    (xs foldLeft(0, 1)) (sumProdIn)
  }

  //Zadanie 5
  def insertsort(xs: List[Int]): List[Int] = {
    def insert(x: List[Int], y: Int): List[Int] = {
      def insertIn(lastHead: List[Int], x: List[Int], y: Int): List[Int] = {
        x match {
          case Nil => lastHead ::: x ::: List(y)
          case h :: t =>
            if (h >= y) lastHead ::: List(y) ::: x
            else insertIn(lastHead ::: List(h), t, y)
        }
      }

      insertIn(Nil, x, y)
    }

    xs match {
      case Nil => Nil
      case List(x) => List(x)
      case xs => (xs foldLeft Nil) (insert)
    }
  }

  def mergesort(list: List[Int]): List[Int] = {
    def split(list: List[Int], newList: List[List[Int]]): List[List[Int]] = {
      list match {
        case Nil => newList
        case h :: t => split(t, (newList ::: List(List(h))))
      }
    }

    def mergeAll(list: List[List[Int]], sortList: List[List[Int]]): List[List[Int]] = {
      def merge(left: List[Int], right: List[Int], tmp: List[Int]): List[Int] = {
        right match {
          case Nil => tmp ::: left
          case hr :: tr =>
            left match {
              case Nil => tmp ::: right
              case hl :: tl =>
                if (hl >= hr) merge(left, tr, tmp ::: List(hr))
                else merge(tl, right, tmp ::: List(hl))
            }
        }
      }

      list match {
        case Nil => sortList
        case h :: t =>
          t match {
            case Nil => sortList ::: List(h)
            case h2 :: t2 => mergeAll(t2, sortList ::: List(merge(h, h2, Nil)))
          }
      }
    }

    def sort(list: List[List[Int]]): List[Int] = {
      val listOfList = mergeAll(list, Nil)
      listOfList match {
        case Nil => Nil
        case List(x) => x
        case _ => sort(listOfList)
      }
    }

    sort(split(list, Nil))
  }


  @main def main() = {
    println(sumProd(List(1, 2, 3, 4)))
    println(insertsort(List(2, 2, 4, 5, 6, 1, 2, 3, 9, 0)))
    println(mergesort(List(9, 8, 7, 6, 5, 4, 3, 2, 1)))
    println(mergesort(List(5, 1, 23, 8, 5, 8, 4, 11, 0, 1, 2)))
  }
}
