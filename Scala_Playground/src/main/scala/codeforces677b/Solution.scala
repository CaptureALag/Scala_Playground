package codeforces677b

import scala.collection.immutable.Queue
import scala.io.StdIn._
import scalaz._, Scalaz._

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

  type PotatoesState[T] = State[PotatoesSituation, T]



  def pushUntilFullOrQueueEmpty: PotatoesState[Unit] = {
    def push: PotatoesState[Unit] = {
      for {
        s@PotatoesSituation(q, foodProc, _) <- get[PotatoesSituation]
        _ <- put({

          s
        })
      } ()
    }

    push
  }

  def readInts: Seq[Int] =
    readLine.split(' ').map(_.toInt)

}
