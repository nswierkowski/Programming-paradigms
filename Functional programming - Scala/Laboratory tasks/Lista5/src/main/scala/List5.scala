import List5.{newPartnership, newPerson, partnership}

object List5 {

  //Zadanie 1
  type point2D = (Double, Double)

  def distance(first : point2D, second : point2D): Double = {
    first match {
      case (a, b) =>
        second match {
          case (c, d) => Math.sqrt(((a - c)*(a - c) + (b - d)*(b - d)))
        }
    }
  }

  type point3D = (Double, Double, Double)

  def distance3D(first: point3D, second: point3D): Double = {
    first match {
      case (a, b, c) =>
        second match {
          case (d, e, f) => Math.sqrt((a-d)*(a-d) + (b - e)*(b - e) + (c - f)*(c - f))
        }
    }
  }

  //Zadanie 2
  type person = (String, String, Int, Boolean, Int)
  type partnership = (person, person)

  def giveMeYounger(partners : partnership): person = {
    partners match {
      case (a, b) =>
        a match {
          case (aName, aSurname, aAge, aSex, aShoesNumber) =>
            b match {
              case (bName, bSurname, bAge, bSex, bShoesNumber) =>
                if (aAge > bAge) b;
                else a
            }
        }
    }
  }

  class newPerson(val name : String, val surName : String, val age : Int, val sex : Boolean, val shoesNumber : Int)
  class newPartnership(val first : newPerson, val second : newPerson)

  def newGiveMeYounger(partners : newPartnership): newPerson = {
    val first : newPerson = partners.first
    val second : newPerson = partners.second
    if first.age > second.age then second
    else first
  }

  //Zadanie 3
  enum weekDay():
    case Poniedzialek extends weekDay()
    case Wtorek extends weekDay()
    case Srode extends weekDay()
    case Czwartek extends weekDay()
    case Piatek extends weekDay()
    case Sobota extends weekDay()
    case Niedziela extends weekDay()

  def weekDayToString(day : weekDay): String = {
    day match {
      case weekDay.Poniedzialek => "Poniedziałek"
      case weekDay.Wtorek => "Wtorek"
      case weekDay.Srode => "Środa"
      case weekDay.Czwartek => "Czwartek"
      case weekDay.Piatek => "Piątek"
      case weekDay.Sobota => "Sobota"
      case weekDay.Niedziela => "Niedziela"
    }
  }

  def nextDay(day: weekDay): weekDay = {
    day match {
      case weekDay.Poniedzialek => weekDay.Wtorek
      case weekDay.Wtorek => weekDay.Srode
      case weekDay.Srode => weekDay.Czwartek
      case weekDay.Czwartek => weekDay.Piatek
      case weekDay.Piatek => weekDay.Sobota
      case weekDay.Sobota => weekDay.Niedziela
      case weekDay.Niedziela => weekDay.Poniedzialek
    }
  }

  //Zadanie 4
  enum maybe[A]():
    case Just(a : A) extends maybe[A]
    case Nothing() extends maybe[A]

  def safeHead[A](list : List[A]): maybe[A] = {
    list match {
      case Nil => maybe.Nothing()
      case h::t => maybe.Just(h)
    }
  }

  //Zadanie 5
  type cuboid = (Double, Double, Double)
  type cone = (Double, Double)
  type sphere = (Double)
  type cylinder = (Double, Double)
    enum solidFigure():
      case  Cuboid (c: cuboid) extends solidFigure
      case  Cone (c: cone) extends solidFigure
      case  Sphere (s : sphere) extends solidFigure
      case  Cylinder (c: cylinder) extends solidFigure

  def volume(figure: solidFigure): Double = {
    figure match {
      case solidFigure.Cuboid(p) =>
        val (a: Double, b: Double, h: Double) = p
        (a * b * h)
      case solidFigure.Cone(p) =>
        val (r: Double, h: Double) = p
        ((3.14 * r * r) * h) / 3.0
      case solidFigure.Sphere(p) =>
        val (r: Double) = p
        (4.0 * 3.14 * r * r) / 3.0
      case solidFigure.Cylinder(p) =>
        val (r: Double, h: Double) = p
        (3.14 * r * r * h)
    }
  }

  @main def main(): Unit = {
    val x1 : point2D = (1.0, 1.0)
    val x2 : point2D = (0.0, 0.0)
    println(distance(x1, x2))

    val x3 : point3D = (1.0,1.0,1.0)
    val x4 : point3D = (0.0,0.0,0.0)
    println(distance3D(x3, x4))

    //Zadanie 2
    val first : person = ("Jan", "Kowalski", 45, true, 42)
    val second : person = ("Anna", "Kowalski", 46, false, 38)
    val couple : partnership = (first, second)

    println(giveMeYounger(couple))

    val newFirst: newPerson = newPerson("Jan", "Kowalski", 45, true, 42)
    val newSecond: newPerson = newPerson("Anna", "Kowalski", 46, false, 38)
    val newCouple: newPartnership = newPartnership(newFirst, newSecond)

    println(newFirst == newGiveMeYounger(newCouple))


    //Zadanie 3
    val day1 = weekDay.Poniedzialek

    println(weekDayToString(day1))
    println(nextDay(day1))
    //Zadanie 4
    println(safeHead[Int](List(1,1,1,1,1)))
    println(safeHead[Int](List()))
    //Zadanie 5
    val par = (1.0d,1.0d,1.0d,1.0d)

    val c : cuboid = (1.0d, 1.0d, 1.0d)
    val figure1 : solidFigure = solidFigure.Cuboid(c)

    val co : cone = (1.0d, 1.0d)
    val figure2 : solidFigure = solidFigure.Cone(co)

    val s : sphere = (1.0d)
    val figure3 : solidFigure = solidFigure.Sphere(s)

    val cy : cylinder = (1.0d, 1.0d)
    val figure4 : solidFigure = solidFigure.Cylinder(cy)

    println(volume(figure1))
    println(volume(figure2))
    println(volume(figure3))
    println(volume(figure4))
  }
}
