package codeforces677b

import scala.collection.immutable.Queue
import scala.io.StdIn._
import scalaz._, Scalaz._
import FoodProcessorLogSupport._

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

        PotatoesSituation(Queue(potatoes: _*), foodProcessor.point[FoodProcLogger], 0)
    }

  type PotatoesState[T] = StateT[FoodProcLogger, PotatoesSituation, T]
  def PotatoesState[T](f : PotatoesSituation => FoodProcLogger[(PotatoesSituation, T)]) = StateT[FoodProcLogger, PotatoesSituation, T](f)
  //def PotatoesState[T](f : PotatoesSituation => (PotatoesSituation, T)) : PotatoesState[T] = State[PotatoesSituation, T](f).point[FoodProcLogger]

  def pushUntilFullOrQueueEmpty: PotatoesState[Unit] = {
    def tryPushOnce = PotatoesState[Boolean] {
      case situation@PotatoesSituation(+:(potato, tail), foodProc, _) =>

        foodProc.map(
          _.tryPutPotato(potato) match {
            case \/-(newFoodProc) =>
              situation.copy(
                tail,
                newFoodProc
              ) -> true

            case -\/(_) =>
              situation -> false
          }
        )

      case situation =>
        (situation -> false).point[FoodProcLogger]
    }

    PotatoesState[Unit] {
      case situation =>
        (situation, ()).point[FoodProcLogger]
    }
  }

  def readInts: Seq[Int] =
    readLine.split(' ').map(_.toInt)

}
