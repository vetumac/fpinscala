case class Container[-A]()

class A

class B extends A

val a = Container[A]()

val b = Container[B]()

a.isInstanceOf[Container[B]]

List