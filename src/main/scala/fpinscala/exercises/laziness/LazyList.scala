package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = {
    this match
      case Empty => List.empty[A]
      case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = {
    this match
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
      case _ => Empty
  }

  def drop(n: Int): LazyList[A] = {
    this match
      case Empty => Empty
      case Cons(h, t) if n == 0 => Cons(h, t)
      case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): LazyList[A] = {
    this match
      case Empty => Empty
      case Cons(h, t) =>
        if(p(h())) {
          Cons(h, () => t().takeWhile(p))
        } else {
          Cons(h, t)
        }
  }

  def forAll(p: A => Boolean): Boolean = {
    this match
      case Empty => true
      case Cons(h, t) => {
        if(p(h())) {
          t().forAll(p)
        } else false
      }
  }

  def headOption: Option[A] = {
    this match
      case Empty => None
      case Cons(h, t) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](p: A => B): LazyList[B] = {
    foldRight(LazyList.empty[B])((a, acc) => Cons(() => p(a), () => acc))
  }

  def filter(p: A => Boolean): LazyList[A] = {
    foldRight(LazyList.empty[A])((a, acc) => if(p(a)) then Cons(() => a, () => acc) else acc)
  }

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight[LazyList[A2]](that)((a, acc) => LazyList.cons[A2](a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = {
    foldRight(LazyList.empty[B])((a, acc) => f(a).append(acc) )
  }

  def startsWith[B](s: LazyList[B]): Boolean = {
    (this, s) match
      case (Empty, Empty) => true
      case (_, Empty) => true
      case (Empty, _) => false
      case (Cons(h, t), Cons(h1, t1)) => h() == h1() && t().startsWith(t1())
  }


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
