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

        PotatoesSituation(Queue(potatoes: _*), foodProcessor)
    }

  type PotatoesState[T] = StateT[FoodProcLogger, PotatoesSituation, T]
  def PotatoesState[T](f : PotatoesSituation => FoodProcLogger[(PotatoesSituation, T)]) = StateT[FoodProcLogger, PotatoesSituation, T](f)
  def PotatoesState2[T](f : PotatoesSituation => (PotatoesSituation, T)) : PotatoesState[T] =
    StateT[FoodProcLogger, PotatoesSituation, T](x => f(x).point[FoodProcLogger])


  println("kak")

  val afterPush: FoodProcLogger[PotatoesSituation] = pushUntilFullOrQueueEmpty.exec(initSituation)

  println(afterPush.written.map(_._1).toList)
  println(afterPush.value)

  val afterProcess = afterPush.flatMap(sit => sit.foodProcessor.process.map(proc => sit.copy(foodProcessor = proc)))

  println(afterProcess.written.map(_._1).toList)
  println(afterProcess.value)

  println("kek")


  def fillAndProcessOnce : PotatoesState[Unit] = {
    for {
      b <- pushUntilFullOrQueueEmpty
    } yield ()
  }


  def pushUntilFullOrQueueEmpty: PotatoesState[Boolean] = {
    def tryPushOnce = PotatoesState[Option[Boolean]] {
      case situation@PotatoesSituation(+:(potato, tail), foodProc) =>

        foodProc.tryPutPotato(potato) match {
          case \/-(newFoodProc : FoodProcessorLogged) =>

            newFoodProc.map(newProc =>
              PotatoesSituation(
                tail,
                newProc
              ) -> Some(true)
            )

          case -\/(newFoodProc : FoodProcessorLogged) =>
            newFoodProc.map(_ => situation -> Some(false))
        }

      case situation =>
        (situation -> none[Boolean]).point[FoodProcLogger]
    }

    PotatoesState[Boolean] { situation : PotatoesSituation =>
      Stream.continually(())
        .scanLeft((situation -> true.some).point[FoodProcLogger])({
          case (accum, _) =>
            accum >>= {
              case (sit, _) =>
                tryPushOnce.run(sit)
            }
        })
        .find(_.value._2.forall(!_)).get
        .map(((b : Option[Boolean]) => b.isDefined).second)
    }
  }

  def readInts: Seq[Int] =
    readLine.split(' ').map(_.toInt)

}
