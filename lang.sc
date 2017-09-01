val a: Option[Int] = Some(3)
val b: Option[Int] = Some(3)
val c: Option[Int] = Some(3)

val result =
  a.flatMap(aa => b.flatMap(bb => c.map(cc => aa + bb + cc)))