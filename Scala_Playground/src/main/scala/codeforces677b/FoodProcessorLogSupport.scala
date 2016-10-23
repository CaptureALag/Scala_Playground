package codeforces677b

import scalaz._, Scalaz._

object FoodProcessorLogSupport {
  type FoodProcessorWriter[T] = Writer[DList[(String, FoodProcessor)], T]
  type FoodProcessorLogged = FoodProcessorWriter[FoodProcessor]

  implicit def logSupport(proc: FoodProcessor): LogSupport = LogSupport(proc)

  case class LogSupport(proc: FoodProcessor) {
    def <|~(msg: String): FoodProcessorLogged =
      Writer(
        DList((msg, proc)),
        proc
      )
  }
}
