package fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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

  val x: Int = List(1, 2, 3, 4, 5) match {
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

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new ArrayIndexOutOfBoundsException("List is empty")
    case Cons(_, l) => l
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0)
      l
    else
      drop(tail(l), n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumLeft(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def productLeft(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)

  def lengthLeft[A](xs: List[A]): Int = foldLeft(xs, 0)((acc, _) => acc + 1)

  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, List[A]())((acc, curr) => Cons(curr, acc))

  def appendFold[A](xs: List[A], appendee: List[A]): List[A] =
    foldRight(xs, appendee)(Cons(_, _))

  def flatten[A](xs: List[List[A]]): List[A] =
    foldRight(xs, Nil: List[A])(append)

  def add1(xs: List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doublesToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map_2[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new ListBuffer[B]

    @tailrec
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }

    go(l)
    List(buf.toList: _*)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new ListBuffer[A]

    @tailrec
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) =>
        if (f(h)) buf += h
        go(t)
    }

    go(l)
    List(buf.toList: _*)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if(f(a)) List(a) else Nil)

  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startsWith(l, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }
}

object ListDataStructure extends App {
  override def main(args: Array[String]): Unit = {
    import List._
    val xs = List(0, 1, 2, 3, 4)
    val xs2 = List(5, 6, 7, 8)
    val ds: List[Double] = List(1.0, 2.0, 3.0)
    println(tail(xs))

    try {
      tail(Nil)
    } catch {
      case e: ArrayIndexOutOfBoundsException => println(e.getMessage)
    }

    println(drop(xs, 2))
    println(init(xs))
    println(sumLeft(xs))
    println(sum(xs))
    println(reverse(xs))
    println(appendFold(xs, xs2))
    println(List(xs, xs2))
    println(flatten(List(xs, xs2)))
    println(add1(xs))
    println(doublesToString(ds))
    println(map(ds)(_.toInt))
    println(filter(xs)(_ % 2 != 0))
    println(filterViaFlatMap(xs)(_ % 2 != 0))
    println(addPairWise(xs, xs2))

    val main = List(1,2,3,4)
    val sub1 = List(1,2)
    val sub2 = List(2,3)
    val sub3 = List(3,4)
    val sub4 = List(4)
    val notsub = List(5)
    val notsub2 = List(3,2,1)

    println(hasSubsequence(main, sub1))
    println(hasSubsequence(main, sub2))
    println(hasSubsequence(main, sub3))
    println(hasSubsequence(main, sub4))
    println(hasSubsequence(main, notsub))
    println(hasSubsequence(main, notsub2))
  }
}
