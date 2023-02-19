import java.util

object list6 {

  def whileLoop(condition : => Boolean)(expression : =>Unit): Unit ={
    if(condition) {
      expression;
      whileLoop(condition)(expression)
    }
  }

  def con(count : Int): Boolean = {count < 3}

//Zad2

//swap
//let swap tab i j =
//let aux = tab.(i) in tab.(i) <- tab.(j); tab.(j) <- aux;;

def swap[A](tab : Array[A], i : Int, j : Int): Unit = {
  var aux = tab(i)
  tab(i) = tab(j)
  tab(j) = aux
}

def partition(tab : Array[Int], l : Int, r : Int): (Int, Int) = {
  var i = l
  var j = r
  var pivot = tab((l+r)/2)
  println(pivot)
  while (i <= j) {
    while tab(i) < pivot do
      i += 1
    while pivot < tab(j) do
      j -= 1
    if (i <= j) {
      swap[Int](tab, i, j)
      i += 1
      j -= 1
    }
  }
  (i,j)
}

def quick(tab : Array[Int], l : Int, r : Int): Unit = {
  if(l < r) {
    var (i,j) = partition(tab, l, r)
    if(j-l < r-i) {
      quick(tab, l, j)
      quick(tab, i, r)
    }
    else{
      quick(tab, i, r)
      quick(tab, l, j)
    }
  }
  else ()
}

def quicksort(tab : Array[Int]): Unit = {quick(tab, 0, (tab.length-1))}

def show[A](tab: Array[A], len : Int): Unit = {
  def printTab[A](i : Int): Unit = {
    if(i < len){
     print(tab(i))
     printTab[A](i+1)
    }
  }
  printTab[A](0)
  println()
}

  @main def main(): Unit = {
    var count = 0
    whileLoop(count < 3) {
      println(count)
      count += 1
    }

    var tab: Array[Int] = new Array[Int](5)
    tab(0) = 8
    tab(1) = 7
    tab(2) = 4
    tab(3) = 5
    tab(4) = 3
    quicksort(tab)
    show(tab, tab.length)
  }
}
