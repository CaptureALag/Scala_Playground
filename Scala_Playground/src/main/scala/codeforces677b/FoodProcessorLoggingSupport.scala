package codeforces677b

import scalaz._, Scalaz._

object FoodProcessorLoggingSupport {
  type FoodProcLogger[T] = Writer[DList[(String, FoodProcessor)], T]
  
  type FoodProcessorLogged = FoodProcLogger[FoodProcessor]

  implicit def logSupport(proc: FoodProcessor): LogSupport = LogSupport(proc)

  case class LogSupport(proc: FoodProcessor) {
    def <|~(msg: String): FoodProcessorLogged =
      Writer(
        DList((msg, proc)),
        proc
      )
  }
}
