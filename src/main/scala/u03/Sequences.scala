package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import scala.collection.View.Empty

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    /*
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()
    */
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => flatMap(l)(element => Cons(mapper(element), Nil()))
      case Nil()      => Nil()

    /*
    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()
    */
    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) => flatMap(l1)(element => if pred.apply(element) then Cons(element, Nil()) else Nil())
      case Nil() => Nil()
    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), zip(t, t2))
      case _ => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(head, tail) if n > 0 => Cons(head, take(tail)(n - 1))
      case Cons(_, _) => Nil()
      case Nil() => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(head, tail) => Cons(head, concat(tail, l2))
      case Nil() => l2

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(head, tail) => concat(mapper(head), flatMap(tail)(mapper)) 
      case Nil() => Nil()
    

    import Optional.*
    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(head, tail) => if filter(tail)(_ < head) != Nil() then min(tail) else Just(head)   
      case Nil() => Optional.Empty()
    
    
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
