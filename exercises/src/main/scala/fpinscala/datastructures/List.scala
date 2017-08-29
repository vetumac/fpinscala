package fpinscala.datastructures

import java.util.NoSuchElementException

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw NoSuchElementException
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }


  def init[A](l: List[A]): List[A] = {
    def loop(in: List[A], res: List[A]): List[A] = {
      in match {
        case Cons(_, Nil) => res
        case Cons(h, t) => loop(t, Cons(h, res))
      }
    }

    l match {
      case Nil => throw IllegalArgumentException
      case Cons(_, _) => loop(l, Nil)
    }
  }


  def length[A](l: List[A]): Int = foldRight(l, 0)((_, n) => 1 + n)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, xs) => foldLeft(xs, f(h, z))(f)
    }


  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def reserse[A](l: List[A]) = foldLeft(l, List[A]())((ls: List[A], i: A) => Cons(i, ls))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, z)()
  }


  def inc(list: List[Int]): List[Int] = foldLeft(list, Nil: List[Int])((l, n) => Cons(n + 1, l))

  def toString(list: List[Double]): List[String] = foldLeft(list, Nil: List[String])((l, n) => Cons(n.toString, l))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldLeft(l, Nil: List[B])((t, h) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldLeft(as, Nil: List[A])((t, h) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldLeft(map(as)(f), Nil: List[B])(append)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zp(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zp(t1, t2))
      case _ => Nil
    }

  def zipWith[A](a: List[A], b: List[A], f: (A, A) => A): List[A] =
    (a, b) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
      case _ => Nil
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startWith(subj: List[A], obj: List[A]): Boolean =
      (subj, obj) match {
        case (_, Nil) => true
        case (Cons(sh, st), Cons(oh, ot)) if sh == oh => startWith(st, ot)
        case _ => false
      }

    sup match {
      case Nil => false
      case Cons(_, t) => if (startWith(sup, sub)) true else hasSubsequence(t, sub)
    }
  }
}
