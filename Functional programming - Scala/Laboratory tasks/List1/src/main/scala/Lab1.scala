import scala.collection.mutable

object Lab1 {

    //Funkcje do zadania 3
    def squater(x : Float): Float = {
        x*x
    }

    def division(x: Float, y : Float): Float = {
        x/y
    }

    def max(x: Float, y: Float): Float = {
        if(x > y) x
        else y
    }

    //Funkcja do zadania 4
    def test(x : Int): Boolean = {
        true
    }

    def test1(x: Int): Boolean = {
        if (x % 2 == 0) true
        else false
    }

    def test2(x: Int): Boolean = {
        if (x % 2 == 1) true
        else false
    }

    def test3(x: Int): Boolean = {
        if (x % 5 == 0 || x % 2 == 0) true
        else false
    }

    def sum(x : Int, y : Int, z : Int, test: Int => Boolean): Int ={
        var sum = 0;
        if (test(x)) sum = sum + x;
        if (test(y)) sum = sum + y;
        if (test(z)) sum = sum + z;
        return sum
    }

    @main def helloInteractive(): Unit = {
        // Zadanie 1
        println("Zadanie 1: ")
        val first = 0.5 + 0.5
        print("first = ")
        println(first)

        val second = 5.0 - 3.0
        print("second = ")
        println(second)

        val third = 0.5 * 6.0
        print("third = ")
        println(third)

        val fourth = 20.0/5.0
        print("fourth = ")
        println(fourth)

        // Zadanie 2
        println("Zadanie 2: ")
        val value = 1.0
        var variable = 1.0
        print("value = ")
        println(value)
        print("variable = ")
        println(variable)


        print("value = ")
        println(value)

        variable = 2.0
        print("variable = ")
        println(variable)

        // Zadanie 3
        println("Zadanie 3: ")
        print("Squater of 25.0 is ")
        println(squater(25.0))

        print("Division of 5.0 and 2.0 is ")
        println(division(5.0, 2.0))

        print("Max of 5.0 and 2.0 is ")
        println(division(5.0, 2.0))

        // Zadanie 4
        print("Sum of 5, 6, 8 is (if adding only even number) ")
        println(sum(5, 6, 8, test1))
        print("Sum of 5, 6, 8 is (if adding only NOT even number) ")
        println(sum(5, 6, 8, test2))
        print("Sum of 5, 6, 8 is (if adding only multiples of five or two number) ")
        println(sum(5, 6, 8, test3))
    }
}
