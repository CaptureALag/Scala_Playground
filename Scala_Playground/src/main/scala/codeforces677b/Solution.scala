package codeforces677b

import scala.collection.immutable.Queue
import scala.io.StdIn._
import scalaz._, Scalaz._
import FoodProcessorLoggingSupport._

object Solution extends App {

  println("Hello world")

  val initSituation : PotatoesSituation =
    (readInts, readInts) match {
      case (Seq(_, procHeight, procSpeed), potatoHeights) =>

        val potatoes = potatoHeights.zipWithIndex.map({
          case (h, i) => new Potato(i, h, h)
        })

        val foodProcessor =
          new FoodProcessor(
            height = procHeight,
            speed = procSpeed
          )

        PotatoesSituation(Queue(potatoes: _*), foodProcessor, 0)
    }

  type PotatoesState[T] = StateT[FoodProcLogger, PotatoesSituation, T]
  def PotatoesState[T](f : PotatoesSituation => FoodProcLogger[(PotatoesSituation, T)]) = StateT[FoodProcLogger, PotatoesSituation, T](f)
  def PotatoesState2[T](f : PotatoesSituation => (PotatoesSituation, T)) : PotatoesState[T] =
    StateT[FoodProcLogger, PotatoesSituation, T](x => f(x).point[FoodProcLogger])


  println("kak")

  val afterPush: FoodProcLogger[PotatoesSituation] = pushUntilFullOrQueueEmpty.exec(initSituation)

  println(afterPush.written.map(_._1).toList)
  println(afterPush.value)

  println("kek")

  def pushUntilFullOrQueueEmpty: PotatoesState[Unit] = {
    def tryPushOnce = PotatoesState[Boolean] {
      case situation@PotatoesSituation(+:(potato, tail), foodProc, _) =>

        foodProc.tryPutPotato(potato) match {
          case \/-(newFoodProc : FoodProcessorLogged) =>

            newFoodProc.map(newProc =>
              situation.copy(
                tail,
                newProc
              ) -> true
            )

          case -\/(_) =>
            (situation -> false).point[FoodProcLogger]
        }

      case situation =>
        (situation -> false).point[FoodProcLogger]
    }

    PotatoesState[Unit] { situation : PotatoesSituation =>
      Stream.continually(())
        .scanLeft((situation, true).point[FoodProcLogger])({
          case (x, _) =>
            val sit : PotatoesSituation = x.value._1
            tryPushOnce.run(sit)
        })
        .takeWhile(_.value._2 == true)
        .last
        .map(_._1 -> ())
    }
  }

  def readInts: Seq[Int] =
    readLine.split(' ').map(_.toInt)

}
