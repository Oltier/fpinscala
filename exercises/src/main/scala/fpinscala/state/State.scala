package fpinscala.state

import fpinscala.state.RNG.{Rand, double, ints, ints2, nonNegativeInt, sequence}

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    (if(nextInt == Int.MinValue) nextInt + 1 else nextInt.abs, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRng) = nonNegativeInt(rng)
    (nextInt / (Int.MaxValue.toDouble + 1), nextRng)
  }

  val double2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, xs: List[Int]): (List[Int], RNG) = {
      if(count <= 0)
        (xs, rng)
      else {
        val (x, r1) = rng.nextInt
        go(count - 1, r1, x :: xs)
      }
    }
    go(count, rng, List.empty[Int])
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (a, r1) = f(rng)
      g(a)(r1)
  }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {
  import fpinscala.state.State._
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      }
    )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List.empty[A]))((s, acc) => s.map2(acc)(_ :: _))

  def sequence[S,A](sas: List[State[S,A]]): State[S, List[A]] =
    sas.reverse.foldLeft(unit[S, List[A]](List.empty[A]))((acc, s) => s.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (Coin, Machine(true, candies, coins)) =>
        Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) =>
        Machine(locked = true, candies - 1, coins)
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (_, Machine(_, 0, _)) => s
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}

object Stateness extends App {
  override def main(args: Array[String]): Unit = {
    val seed = 5L
    val rng = RNG.Simple(seed)
    println(rng.nextInt._1)
    println(nonNegativeInt(rng))
    println(double(rng))
    println(ints(5)(rng)._1)
    println(ints2(5)(rng)._1)
    val buyOne: List[Input] = List(Coin, Turn)
    val inputs: List[Input] = List.fill(4)(buyOne).flatten
    val machine1 = Machine(locked = true, 5, 10)
    println(State.simulateMachine(inputs).run(machine1)._1)
  }
}