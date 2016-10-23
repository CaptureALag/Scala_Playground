package codeforces677b

import codeforces677b.FoodProcessorLogSupport.FoodProcessorLogged

import scala.collection.immutable.Queue


case class PotatoesSituation(outerQueue : Queue[Potato], foodProcessor: FoodProcessorLogged, secondsPassed : Int = 0)