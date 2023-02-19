sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

object List4 {

  //Zadanie 3
  def breadthBT[A](tree : BT[A]): List[A] = {
    def addToQueue(queue : List[BT[A]], t1 : BT[A]): List[BT[A]] = {
      t1 match{
        case Empty => queue
        case Node(elem, left, right) => queue:::List(Node(elem, left, right))
      }
    }
    def breadthBTIn(tree : BT[A], list : List[A], queue : List[BT[A]]): List[A] ={
      tree match {
        case Empty =>  list
        case Node(elem, left, right) =>
          val newQueue = addToQueue(addToQueue(queue, left), right)
          newQueue match {
            case Nil => (list:::List(elem))
            case h::t => breadthBTIn(h, (list:::List(elem)), t)
          }
      }
    }
    breadthBTIn(tree, Nil, Nil)
  }

  //Zadanie 4
  def wew[A](tree : BT[A]): Int = {
    def wewIn[A](tree : BT[A], sum : Int): Int = {
      tree match {
        case Empty => sum
        case Node(v, l, r) =>
          (l, r) match {
            case (Empty, Empty) => sum
            case (Empty, Node (v2, l2, r2) ) => sum + wewIn(r, (sum + 1))
            case (Node (v2, l2, r2), Empty) => sum + wewIn(l, (sum + 1))
            case (Node (vl, ll, rl), Node (vr, lr, rr) ) => sum + wewIn(r, (sum + 1)) + wewIn(l, (sum + 1))
          }
      }
    }
    wewIn[A](tree, 0)
  }

  def zew[A](tree : BT[A]): Int = {
    def zewIn[A](tree : BT[A], sum : Int): Int = {
      tree match {
        case Empty => sum
        case Node(v, l, r) => zewIn[A](l, sum+1) + zewIn[A](r, sum+1)
      }
    }
    zewIn[A](tree, 0)
  }

  //Zadanie 5
  def depthSearch(g : Graph[Int], start : Int): List[Int] = {
    def checkWhatsNextElement(newElement : Int, stack : List[Int]): List[Int] = {
      (g succ newElement) match {
        case Nil => checkWhatsNextStack(newElement, stack)
        case h::t => (g succ newElement):::stack
      }
    }
    def checkWhatsNextStack(newElement : Int, stack : List[Int]): List[Int] = {
      stack match {
        case Nil => Nil
        case h::t => stack
      }
    }
    def depthSearchIn(newElement : Int, visited : List[Int], stack : List[Int]): List[Int] = {
      if(visited.contains(newElement)){
        stack match {
          case Nil => visited
          case h::t => depthSearchIn(h, visited, t)
        }
      }
      else {
        val newStack = checkWhatsNextElement(newElement, stack)
        newStack match {
          case Nil => visited:::List(newElement)
          case h::t => depthSearchIn(h, (visited:::List(newElement)), newStack)
        }
      }
    }
    depthSearchIn(start, Nil, Nil)
  }

  @main def main(): Unit = {
    val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
    println(breadthBT[Int](t))

    val tt = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))
    println(wew[Int](tt))
    println(zew[Int](tt))

    val g = Graph((i: Int) => i match
      case 0 => List(3)
      case 1 => List(0, 2, 4)
      case 2 => List(1)
      case 3 => Nil
      case 4 => List(0, 2)
      case n => throw new Exception(s"Graph g: node $n doesn't exist")
    )

    println(depthSearch(g, 4))
  }
}
