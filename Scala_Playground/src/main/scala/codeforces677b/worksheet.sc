import codeforces677b.PotatoesSituation
import codeforces677b.Solution.PotatoesState

import scalaz.{-\/, \/-}

def pushUntilFullOrQueueEmpty: PotatoesState[Unit] = {
  def push = PotatoesState[Boolean] {
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
      ).run

    case situation =>
      situation -> false
  }

  push.map(_ => ())
}