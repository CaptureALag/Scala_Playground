package codeforces677b

import codeforces677b.FoodProcessorLogSupport._

import scalaz.Scalaz._
import scalaz._

class FoodProcessor private(
                             val height: Int,
                             val procPower: Int,
                             val potatoQueue: Dequeue[Potato],
                             val heightLeft: Int
                           ) {

  require(height > 0)
  require(procPower > 0)
  require(heightLeft >= 0)

  def this(height: Int, speed: Int) =
    this(height, speed, Dequeue.empty, height)

  def tryPutPotato(potato: Potato): FoodProcessorLogged \/ FoodProcessorLogged =
    heightLeft match {
      case x if x >= potato.heightLeft =>
        \/-(enqueue(potato))
      case _ =>
        -\/(this <|~ s"Trying to enqueue $potato but no space left")
    }

  def process: FoodProcessorLogged = {
    def _process(accum: FoodProcessorLogged, powerLeft: Int): FoodProcessorLogged = {
      accum.flatMap(proc =>
        proc.dequeue match {
          case (None, res) =>
            res
          case (Some(potato), tail) =>
            powerLeft - potato.heightLeft match {
              case diff if diff > 0 =>
                _process(tail, diff)
              case 0 =>
                tail
              case diff if diff < 0 =>
                val slicedPotato = potato.slice(-diff)
                tail.flatMap(_.enqueueFront(slicedPotato))
            }
        }
      )
    }

    def _this = this
    _process(_this.point[FoodProcessorWriter], this.procPower)
  }

  private def enqueue(potato: Potato): FoodProcessorLogged = {
    require(potato.height == potato.heightLeft)

    copy(
      newQueue = potatoQueue :+ potato,
      newHeightLeft = heightLeft - potato.height
    ) <|~ s"Enqueueing $potato"
  }

  private def dequeue: (Option[Potato], FoodProcessorLogged) = {
    potatoQueue match {
      case +:(potato: Potato, queueTail) =>
        (
          Some(potato),
          copy(
            newQueue = queueTail,
            newHeightLeft = this.heightLeft + potato.heightLeft
          ) <|~ s"Popping $potato"
          )

      case _ =>
        (None, this <|~ "Trying to pop potato, but queue is empty")
    }
  }

  private def enqueueFront(potato: Potato): FoodProcessorLogged = {
    copy(
      newQueue = potato +: potatoQueue,
      newHeightLeft = heightLeft - potato.height
    ) <|~ s"Enqueueing $potato in the front"
  }

  private def copy(newQueue: Dequeue[Potato], newHeightLeft: Int): FoodProcessor =
    new FoodProcessor(this.height, this.procPower, newQueue, newHeightLeft)
}
