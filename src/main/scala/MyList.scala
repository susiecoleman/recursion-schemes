import scalaz.Functor
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._

sealed trait MyListF[F]
case class ConsF[F](head: Int, tail: F) extends MyListF[F]
case class NilF[F]() extends MyListF[F]

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

  def main(args: Array[String]): Unit = {
    println(myList)
  }
}
