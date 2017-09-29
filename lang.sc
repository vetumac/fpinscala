

val a: Option[Int] = Some(3)
val b: Option[Int] = Some(3)
val c: Option[Int] = Some(3)

val result =
  a.flatMap(aa => b.flatMap(bb => c.map(cc => aa + bb + cc)))

val optL = List(Some(1), Some(4), Some(2), None)

optL map (x => x.flatMap(y => Some(y + 1)))

def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a match {
    case Nil => Some(Nil)
    case xOpt :: xsOpt => xOpt.flatMap(x => sequence(xsOpt).map(acc => x :: acc))
  }
}

sequence(optL)

val aa: Either[String, Int] = Right(3)
val bb: Either[String, Int] = Left("errorBB")

for {
  a <- aa
  b <- bb
} yield a + b

val ones: Stream[Int] = Stream.cons(1, ones)

ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1)
!ones.contains(1)