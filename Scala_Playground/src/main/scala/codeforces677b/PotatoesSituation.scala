package codeforces677b

import scala.collection.immutable.Queue


case class PotatoesSituation(outerQueue : Queue[Potato], foodProcessor: FoodProcessor, secondsPassed : Int = 0)