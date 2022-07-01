package Set

enum Set[+A]:
  case Empty
  case NonEmpty private[Set] (a: A, rest: Set[A])

  override def toString(): String =
    @scala.annotation.tailrec
    def go[A](set: Set[A], acc: StringBuilder): String =
      set match
        case Empty =>
          (acc += ')').result
        case NonEmpty(x, tail) =>
          go(
            tail,
            (acc.append(x))
              ++= (if tail != Empty then ", " else "")
          )

    go(this, new StringBuilder("("))

  def size[A](): Long =
    @scala.annotation.tailrec
    def go[A](xs: Set[A], acc: Long = 0): Long =
      xs match
        case Empty => acc
        case NonEmpty(a, rest) =>
          go(rest, 1 + acc)

    go(this)

  def includes[A](a: A): Boolean =
    @scala.annotation.tailrec
    def go[A](xs: Set[A], a: A): Boolean =
      xs match
        case Empty             => false
        case NonEmpty(b, rest) => if (a == b) then true else go(rest, a)
    go(this, a)

object Set:
  def apply[A](xs: A*): Set[A] =
    makeSet[A](xs*)

  def makeSet[A](xs: A*): Set[A] =
    xs.distinct.foldRight(Empty: Set[A])((a, rest) => NonEmpty(a, rest))

  def add[A](xs: Set[A], a: A): Set[A] =
    if xs.includes(a) then xs else NonEmpty(a, xs)

  def map[A, B](xs: Set[A], f: A => B): Set[B] =
    @scala.annotation.tailrec
    def go(xs: Set[A], f: A => B, acc: Set[B] = Empty): Set[B] =
      xs match
        case Empty             => acc
        case NonEmpty(a, rest) => go(rest, f, add(acc, f(a)))
    go(xs, f)

  def areEqual[A](xs: Set[A], ys: Set[A]): Boolean =
    if xs.size() != ys.size() then false
    else {
      @scala.annotation.tailrec
      def go[A](xs: Set[A], ys: Set[A]): Boolean =
        xs match
          case Empty => true
          case NonEmpty(a, rest) =>
            if ys.includes(a) then go(rest, ys) else false

      go(xs, ys)
    }

  @scala.annotation.tailrec
  def remove[A](xs: Set[A], a: A, acc: Set[A] = Empty): Set[A] =
    xs match
      case Empty => acc
      case NonEmpty(b, rest) =>
        if (a == b) then remove(rest, a, acc) else remove(rest, a, add(acc, b))

  @scala.annotation.tailrec
  def union[A](xs: Set[A], ys: Set[A]): Set[A] =
    xs match
      case Empty             => ys
      case NonEmpty(a, rest) => union(rest, add(ys, a))

  @scala.annotation.tailrec
  def difference[A](xs: Set[A], ys: Set[A]): Set[A] =
    ys match
      case Empty => xs
      case NonEmpty(a, rest) =>
        if xs.includes(a) then difference(Set.remove(xs, a), rest)
        else difference(xs, rest)

  def unionBy[A, B, C](xs: Set[A], ys: Set[B])(f: A => C, g: B => C): Set[C] =
    union(map(xs, f), map(ys, g))

  def differenceBy[A, B, C](xs: Set[A], ys: Set[B])(
      f: A => C,
      g: B => C
  ): Set[C] =
    difference(map(xs, f), map(ys, g))

@main def main =
  var A = Set(1, 2, 3)
  var B = Set(1, 3, 5)
  println(A)
  println(B)
  println(Set.union(A, B))
  println(Set.difference(A, B))
  println(
    Set.unionBy(Set("a", "b", "c", "d", "E"), Set("A", "B", "C"))(
      a => a.toLowerCase,
      b => b.toLowerCase
    )
  )

  println(Set.areEqual(A, B))
  println(Set.areEqual(Set(1, 3, 4), Set(3, 4, 1, 2, 3, 3)))
