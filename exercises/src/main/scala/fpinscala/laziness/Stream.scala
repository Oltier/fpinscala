package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  //  def uncons: Option[(A, Stream[A])]
  //  def isEmpty: Boolean = uncons.isEmpty

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop n-1
    case _ => this
  }

  def takeWhile2(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)(
      (h, t) =>
        if (p(h))
          cons(h, t)
        else
          empty
    )

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if(p(h))
        cons(h, t)
      else
        t
    )

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def toList: List[A] = {
    val buf = ListBuffer[A]()

    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  // Still a bit slow due to reverse
  def toListSlow: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List.empty[A]).reverse
  }

  // Stack overflow
  def toList2: List[A] = this match {
    case Cons(h, t) => h() :: t().toList2
    case _ => List()
  }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), empty[B]))
      case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (empty[A], t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_,_))

  def startsWith[B>:A](s2: Stream[B]): Boolean =
    zipAll(s2)
      .takeWhileViaUnfold(_._2.isDefined)
      .forAll(t => t._1 == t._2)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    } append empty

  def hasSubsequence[B>:A](s2: Stream[B]): Boolean =
    tails exists (_ startsWith s2)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) { (acc, p0) =>
      lazy val p1 = p0
      val h: B = p1._1
      val s: Stream[B] = p1._2
      val newAcc = f(acc, h)
      (newAcc, cons(newAcc, s))
    }._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

//  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(h => Some(h, h + 1))
  def fibsViaUnfold: Stream[Int] =
    unfold[Int,(Int, Int)]((0, 1))(f => Some(f._1, (f._2, f._1 + f._2)))
}

object Laziness extends App {
  override def main(args: Array[String]): Unit = {
    val s = Stream(List.range(0, 20): _*)
    println(s.toList)
    println(s.takeWhile(i => i < 10).toList)
    println(s.takeWhile2(i => i < 10).toList)

    println(Stream.ones.exists(_ % 2 != 0))
    println(Stream.ones.map(_ + 1).exists(_ % 2 == 0))
    println(Stream.ones.takeWhile(_ == 1))
    println(Stream.ones.forAll(_ != 1))
    println(Stream.from(5).take(5).toList)
    println(Stream.fibs.take(5).toList)
    println(Stream.ones.take(5).toList)
    println(Stream.onesViaUnfold.take(5).toList)
    println(Stream.constant(2).take(5).toList)
    println(Stream.constantViaUnfold(2).take(5).toList)
    println(Stream.from(5).take(5).toList)
    println(Stream.fromViaUnfold(5).take(5).toList)
    println(Stream.fibs.take(5).toList)
    println(Stream.fibsViaUnfold.take(5).toList)
    println(Stream.fibsViaUnfold.takeViaUnfold(5).toList)

    val s2 = Stream(1, 2, 3)
    println(s2.startsWith(Stream(1,2)))
    println(s2.tails.toList.map(_.toList))
    println(s2.scanRight(0)(_ + _).toList)
  }
}