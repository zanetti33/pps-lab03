package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u03.Sequences.Person.{Student, Teacher}

import scala.annotation.tailrec
import scala.collection.View.Empty

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Sequence:

    extension (l: Sequence[Int]) def sum: Int = l match
      case Cons(h, t) => h + t.sum
      case _          => 0

    /*
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()
    */
    extension [A, B](l: Sequence[A]) def map(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => l.flatMap(element => Cons(mapper(element), Nil()))
      case Nil()      => Nil()

    /*
    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()
    */
    extension [A](l1: Sequence[A]) def filter(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) => flatMap(l1)(element => if pred.apply(element) then Cons(element, Nil()) else Nil())
      case Nil() => Nil()
    // Lab 03
    extension [A, B](first: Sequence[A]) def zip (second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), t.zip(t2))
      case _ => Nil()

    extension [A](l: Sequence[A]) def take(n: Int): Sequence[A] = l match
      case Cons(head, tail) if n > 0 => Cons(head, tail.take(n - 1))
      case Cons(_, _) => Nil()
      case Nil() => Nil()
    
    extension [A](l1: Sequence[A]) def concat (l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(head, tail) => Cons(head, tail.concat(l2))
      case Nil() => l2

    extension [A, B](l: Sequence[A]) def flatMap(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(head, tail) => mapper(head).concat(tail.flatMap(mapper))
      case Nil() => Nil()

    import Optional.*
    extension (l: Sequence[Int]) @tailrec
    def min: Optional[Int] = l match
      case Cons(head, tail) => filter(tail)(_ < head) match
        case Cons(head, tail) => tail.min
        case Nil() => Just(head)
      case Nil() => Optional.Empty()

    extension (l: Sequence[Person]) def courses: Sequence[String] = l match
      case Cons(_, _) => map(filter(l)(_ match
        case Teacher(_, _) => true
        case Student(_, _) => false))(_ match
        case Teacher(_, course) => course)
      case _ => Nil()

    extension [A, B](list: Sequence[A]) @tailrec
    def foldLeft(foldOver: B)(accumulator: (B, A) => B): B = list match
      case Sequence.Cons(head, tail) => tail.foldLeft(accumulator(foldOver, head))(accumulator)
      case Sequence.Nil() => foldOver


@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  val l2 = Sequence.Cons(Person.Student("Lorenzo", 1), Sequence.Cons(Person.Teacher("Viroli", "PPS"), Sequence.Nil()))
  println(Sequence.courses(l2)) // Cons(PPS,Nil())

  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(foldLeft(lst)(0)(_ - _)) // -16
