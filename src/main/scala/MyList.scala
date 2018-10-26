import scalaz.Functor
import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix
//A recursive type
//sealed trait MyList[A]
//case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]
//case class Nil[A]() extends MyList[A]

//Data type - non recursive - no self reference.
//These are the plain data. I.e. there is nothing to stop us using ConsF as a pair so ConsF[Int].
sealed trait MyListF[F]
case class ConsF[F](head: Int, tail: F) extends MyListF[F]
case class NilF[F]() extends MyListF[F]

//This means that if we wanted to define a recursive type MyList we would do
//type Mylist = MyListF[MyListF[MyListF[...
//type MyList = MyListF[MyList]
//The type is infinite

//Adding the recursion these are recursive classes. Self referential.
//These determine how we use the data. So we want to make it recursive.
//case class Cons(head: Int, tail: ConsF[Cons])
//case class Nil(value: NilF[Nil])

//These can be made more general
//Higher kinded types
//case class GenCons[F[_], A](head: A, tail: F[GenCons[F, A]])
//case class GenNil[F[_]](value: F[GenNil[F]])

//case class GenNil[F[_]](value: F[GenNil[F]])
//is actually Fix
//case class Fix[F[_]](unfix: F[Fix[F]])
//This is how to represent an infinite type in a finite way
//case class GenCons[F[_], A](head: A, tail: F[GenCons[F, A]])
//is actually cofree
//case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
//Its the same as fix but it allows you associate a label with each value
//Finally there is also Free which is a free monad when it's an A or an F
//case class Free[F[_], A](resume: A \/ F[Free[F,A]])

//The implementations in cats, scalaz etc need to be lazy and not cause stack overflows so the
//implementations are more challenging.


object Main {
  type MyListType = Fix[MyListF]

  val list: MyListType = Fix(
    ConsF(1,
      Fix(
        ConsF(2,
          Fix(
            NilF[Fix[MyListF]]()
          )
        )
      )
    )
  )

  implicit val listFunctor: Functor[MyListF] = new Functor[MyListF] {
    def map[A,B](fa: MyListF[A])(f: A => B): MyListF[B] =
      fa match {
        case NilF() => NilF()
        case ConsF(h, t) => ConsF(h, f(t))
      }
  }

  def main(args: Array[String]): Unit = {
    println(myList)
  }

  type Input = List[Int]

  val build: Coalgebra[MyListF, Input] = {
    case list =>
      if(list.nonEmpty) {
        ConsF(list.head*2, list.tail)
      } else {
        NilF()
      }
  }

  val myList: MyListType = List(12,13).ana[MyListType](build)





}
